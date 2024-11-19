use anyhow::anyhow;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use similar::{DiffOp, TextDiff};
use std::{
    fs::File,
    io::{BufRead, BufReader},
    os::unix::net::UnixStream,
    thread,
};
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

fn position_from_kak_selection_desc(desc: &str) -> anyhow::Result<Position> {
    let (line, character) = desc.split_once(".").ok_or(anyhow!("invalid position"))?;
    Ok(Position {
        line: line.parse()?,
        character: character.parse()?,
    })
}

fn range_from_kak_selection_desc(desc: &str) -> anyhow::Result<Range> {
    let (start, end) = desc.split_once(",").ok_or(anyhow!("invalid range"))?;
    Ok(Range {
        start: position_from_kak_selection_desc(start)?,
        end: position_from_kak_selection_desc(end)?,
    })
}

fn ranges_from_kak_selection_desc(desc: &str) -> anyhow::Result<Vec<Range>> {
    desc.split(" ")
        .map(range_from_kak_selection_desc)
        .try_collect()
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

struct FifoLines {
    fifo_path: String,
    fifo_lines: std::io::Lines<BufReader<File>>,
}

impl FifoLines {
    fn new(fifo_path: String) -> std::io::Result<FifoLines> {
        let fifo_lines = BufReader::new(File::open(&fifo_path)?).lines();
        Ok(FifoLines {
            fifo_path,
            fifo_lines,
        })
    }
}

impl Iterator for FifoLines {
    type Item = std::io::Result<String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.fifo_lines.next() {
            Some(line) => Some(line),
            None => match File::open(&self.fifo_path) {
                Ok(fifo) => {
                    self.fifo_lines = BufReader::new(fifo).lines();
                    self.next()
                }
                Err(e) => Some(Err(e)),
            },
        }
    }
}

#[derive(Debug)]
enum MessageFromEditor {
    BufferChanged {
        file_path: String,
        new_content: String,
    },
    BufferCreated {
        file_path: String,
    },
    CursorMoved {
        file_path: String,
        cursors: Vec<Range>,
    },
}

struct EditorMessages {
    fifo_lines: FifoLines,
}

impl EditorMessages {
    fn new(fifo_path: String) -> std::io::Result<Self> {
        Ok(EditorMessages {
            fifo_lines: FifoLines::new(fifo_path)?,
        })
    }
}

impl Iterator for EditorMessages {
    type Item = anyhow::Result<MessageFromEditor>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.fifo_lines.next() {
            Some(Ok(message)) => match message.as_str() {
                "BufferChanged" => {
                    fn read_buffer_changed_message(
                        fifo_lines: &mut FifoLines,
                    ) -> anyhow::Result<MessageFromEditor> {
                        let file_path = fifo_lines
                            .next()
                            .ok_or(anyhow!("invalid BufferChanged message"))??;
                        let num_lines: usize = fifo_lines
                            .next()
                            .ok_or(anyhow!("invalid BufferChanged message"))??
                            .parse()?;
                        let content: Vec<String> = fifo_lines.take(num_lines).try_collect()?;
                        if content.len() < num_lines {
                            return Err(anyhow!("invalid BufferChanged message"));
                        }
                        let content = content.join("\n");
                        Ok(MessageFromEditor::BufferChanged {
                            new_content: content,
                            file_path,
                        })
                    }
                    Some(read_buffer_changed_message(&mut self.fifo_lines))
                }
                "BufferCreated" => match self.fifo_lines.next() {
                    Some(Ok(file_path)) => Some(Ok(MessageFromEditor::BufferCreated { file_path })),
                    _ => Some(Err(anyhow!("invalid BufferCreated message"))),
                },
                "CursorMoved" => {
                    fn read_cursor_moved_message(
                        fifo_lines: &mut FifoLines,
                    ) -> anyhow::Result<MessageFromEditor> {
                        let file_path = fifo_lines
                            .next()
                            .ok_or(anyhow!("invalid CursorMoved message"))??;
                        let cursors = fifo_lines
                            .next()
                            .ok_or(anyhow!("invalid CursorMoved message"))??;
                        Ok(MessageFromEditor::CursorMoved {
                            file_path,
                            cursors: ranges_from_kak_selection_desc(&cursors)?,
                        })
                    }
                    Some(read_cursor_moved_message(&mut self.fifo_lines))
                }
                _ => Some(Err(anyhow!("unknown message"))),
            },
            Some(Err(e)) => Some(Err(e.into())),
            None => None,
        }
    }
}

fn listen_to_editor_messages() -> anyhow::Result<()> {
    let fifo_path = "/tmp/ethersync-kak-fifo";
    let mut prev_content = "".to_string();
    for message in EditorMessages::new(fifo_path.to_string())? {
        let relayed_message = match message? {
            MessageFromEditor::BufferChanged {
                file_path,
                new_content,
            } => {
                let relayed = EditorProtocolMessageFromEditor::Edit {
                    delta: EditorTextDelta::from_diff(&prev_content, &new_content),
                    uri: "file://".to_owned() + &file_path,
                    revision: 42,
                };
                prev_content = new_content;
                relayed
            }
            MessageFromEditor::BufferCreated { file_path } => {
                EditorProtocolMessageFromEditor::Open {
                    uri: "file://".to_owned() + &file_path,
                }
            }
            MessageFromEditor::CursorMoved { file_path, cursors } => {
                EditorProtocolMessageFromEditor::Cursor {
                    uri: "file://".to_owned() + &file_path,
                    ranges: cursors,
                }
            }
        };
        println!(
            "{}",
            serde_json::to_string(&JSONRPCFromEditor::Request {
                jsonrpc: JSONRPCVersionTag,
                id: 42,
                payload: relayed_message
            })?
        );
    }
    Ok(())
}

fn listen_to_daemon_messages() -> anyhow::Result<()> {
    let socket_path = "/tmp/ethersync";
    let stream = UnixStream::connect(socket_path)?;
    let stream = BufReader::new(stream);
    for line in stream.lines() {
        let line = line.unwrap();
        let message: serde_json::Result<EditorProtocolMessageToEditor> =
            serde_json::from_str(&line);
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
