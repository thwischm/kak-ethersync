use serde::{Deserialize, Serialize};
use similar::{utils::TextDiffRemapper, DiffOp, TextDiff};
use std::{
    fs::{self, File},
    iter,
    os::unix::net::UnixStream,
};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
pub enum EditorProtocolMessageToEditor {
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
pub struct EditorProtocolMessageError {
    pub code: i32,
    pub message: String,
    pub data: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum JSONRPCResponse {
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum JSONRPCFromEditor {
    Request {
        id: usize,
        #[serde(flatten)]
        payload: EditorProtocolMessageFromEditor,
    },
    Notification {
        #[serde(flatten)]
        payload: EditorProtocolMessageFromEditor,
    },
}

impl JSONRPCFromEditor {
    pub fn from_jsonrpc(jsonrpc: &str) -> Result<Self, anyhow::Error> {
        let message = serde_json::from_str(jsonrpc)?;
        Ok(message)
    }
}

type DocumentUri = String;
type CursorId = String;

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EditorTextOp {
    pub range: Range,
    pub replacement: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EditorTextDelta(pub Vec<EditorTextOp>);

impl EditorTextDelta {
    fn from_diff(old: &str, new: &str) -> Self {
        let line_ends: Vec<_> = old.match_indices('\n').map(|(i, _)| i).collect();
        let index_to_position = |i| {
            let line = line_ends.partition_point(|&x| x < i);
            let start_of_current_line = if line == 0 {0} else {line_ends[line - 1] + 1};
            Position {
                line,
                character: i - start_of_current_line,
            }
        };

        let diff = TextDiff::from_chars(old, new);
        dbg!(diff.ops());
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
                    replacement: new[new_index..new_index + new_len].to_string(),
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
                    replacement: new[new_index..new_index + new_len].to_string(),
                }),
            })
            .collect();
        Self(text_ops)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "method", content = "params", rename_all = "camelCase")]
pub enum EditorProtocolMessageFromEditor {
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

fn main() -> anyhow::Result<()> {
    // let socket_path = "/tmp/ethersync";
    // let stream = UnixStream::connect(socket_path)?;
    // let stream = BufReader::new(stream);
    // for line in stream.lines() {
    //     let line = line.unwrap();
    //     let message: serde_json::Result<EditorProtocolMessageToEditor> = serde_json::from_str(&line);
    //     println!("{:#?}", message)
    // }
    // Ok(())
    let fifo_path = "/tmp/ethersync-kak-fifo";
    let mut prev_version = "".to_string();
    for new_version in Versions::new(fifo_path.to_string()) {
        println!(
            "{:#?}",
            EditorTextDelta::from_diff(&prev_version, &new_version)
        );
        prev_version = new_version;
    }
    Ok(())
}
