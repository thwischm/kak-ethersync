use itertools::Itertools;
use std::{io::{BufRead, BufReader}, os::unix::net::UnixStream, thread};
use serde::{Deserialize, Serialize};
use similar::{DiffOp, TextDiff};
use std::fs;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
enum EditorProtocolMessageToEditor {
    Edit {
        uri: DocumentUri,
        revision: usize,
        delta: EditorTextDelta,
    },
    Cursor {
        userid: CursorId,
        name: Option<String>,
        uri: DocumentUri,
        ranges: Vec<Range>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct EditorProtocolMessageError {
    code: i32,
    message: String,
    data: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
enum JSONRPCResponse {
    RequestSuccess {
        id: usize,
        result: String,
    },
    RequestError {
        // id must be Null if there was an error detecting the id in the Request Object.
        id: Option<usize>,
        error: EditorProtocolMessageError,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct JSONRPCVersionTag;

impl Serialize for JSONRPCVersionTag {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str("2.0")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(untagged)]
enum JSONRPCFromEditor {
    Request {
        jsonrpc: JSONRPCVersionTag,
        id: usize,
        #[serde(flatten)]
        payload: EditorProtocolMessageFromEditor,
    },
    Notification {
        jsonrpc: JSONRPCVersionTag,
        #[serde(flatten)]
        payload: EditorProtocolMessageFromEditor,
    },
}

type DocumentUri = String;
type CursorId = String;

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
struct Position {
    line: usize,
    character: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct Range {
    start: Position,
    end: Position,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct EditorTextOp {
    range: Range,
    replacement: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct EditorTextDelta(Vec<EditorTextOp>);

impl EditorTextDelta {
    fn from_diff(old: &str, new: &str) -> Self {
        let old = UnicodeSegmentation::graphemes(old, true).collect_vec();
        let new = UnicodeSegmentation::graphemes(new, true).collect_vec();

        let line_ends: Vec<_> = old
            .iter()
            .positions(|&s| s == "\n" || s == "\r\n")
            .collect();
        let index_to_position = |i| {
            let line = line_ends.partition_point(|&x| x < i);
            let start_of_current_line = if line == 0 {
                0
            } else {
                line_ends[line - 1] + 1
            };
            Position {
                line,
                character: i - start_of_current_line,
            }
        };

        let diff = TextDiff::from_slices(&old, &new);
        let text_ops = diff
            .ops()
            .iter()
            .filter_map(|op| match *op {
                DiffOp::Equal { .. } => None,
                DiffOp::Delete {
                    old_index,
                    old_len,
                    new_index: _,
                } => Some(EditorTextOp {
                    range: Range {
                        start: index_to_position(old_index),
                        end: index_to_position(old_index + old_len),
                    },
                    replacement: "".to_string(),
                }),
                DiffOp::Insert {
                    old_index,
                    new_index,
                    new_len,
                } => Some(EditorTextOp {
                    range: Range {
                        start: index_to_position(old_index),
                        end: index_to_position(old_index),
                    },
                    replacement: new[new_index..new_index + new_len].join(""),
                }),
                DiffOp::Replace {
                    old_index,
                    old_len,
                    new_index,
                    new_len,
                } => Some(EditorTextOp {
                    range: Range {
                        start: index_to_position(old_index),
                        end: index_to_position(old_index + old_len),
                    },
                    replacement: new[new_index..new_index + new_len].join(""),
                }),
            })
            .collect();
        Self(text_ops)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
enum EditorProtocolMessageFromEditor {
    Open {
        uri: DocumentUri,
    },
    Close {
        uri: DocumentUri,
    },
    Edit {
        uri: DocumentUri,
        revision: usize,
        delta: EditorTextDelta,
    },
    Cursor {
        uri: DocumentUri,
        ranges: Vec<Range>,
    },
}

struct Versions {
    fifo_path: String,
}

impl Versions {
    fn new(fifo_path: String) -> Versions {
        Versions { fifo_path }
    }
}

impl Iterator for Versions {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        Some(fs::read_to_string(&self.fifo_path).expect("could not open fifo"))
    }
}

fn listen_to_editor_messages() -> anyhow::Result<()> {
    let fifo_path = "/tmp/ethersync-kak-fifo";
    let mut prev_version = "".to_string();
    for new_version in Versions::new(fifo_path.to_string()) {
        println!(
            "{}",
            serde_json::to_string(&JSONRPCFromEditor::Request {
                jsonrpc: JSONRPCVersionTag,
                id: 42,
                payload: EditorProtocolMessageFromEditor::Edit {
                    delta: EditorTextDelta::from_diff(&prev_version, &new_version),
                    uri: "todo".to_string(),
                    revision: 42
                }
            })?
        );
        prev_version = new_version;
    }
    Ok(())
}

fn listen_to_daemon_messages() -> anyhow::Result<()> {
    let socket_path = "/tmp/ethersync";
    let stream = UnixStream::connect(socket_path)?;
    let stream = BufReader::new(stream);
    for line in stream.lines() {
        let line = line.unwrap();
        let message: serde_json::Result<EditorProtocolMessageToEditor> = serde_json::from_str(&line);
        println!("{:#?}", message)
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let editor_thread = thread::spawn(listen_to_editor_messages);
    let daemon_thread = thread::spawn(listen_to_daemon_messages);
    editor_thread.join().expect("couldn't join editor thread")?;
    daemon_thread.join().expect("couldn't join daemon thread")?;
    Ok(())
}
