use anyhow::anyhow;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use similar::{DiffOp, TextDiff};
use std::{
    fs::File,
    io::{BufRead, BufReader, Read, Write},
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, PartialOrd, Ord)]
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

    fn sequential_ops(&self, document: &str) -> Vec<EditorTextOp> {
        let mut old_position = Position {
            line: 0,
            character: 0,
        };
        let mut new_position = Position {
            line: 0,
            character: 0,
        };
        let mut grapheme_iter = document.graphemes(true);
        let mut result = Vec::new();
        for edit in self.0.iter() {
            while old_position < edit.range.start {
                let char = grapheme_iter.next();
                if char == Some("\n") {
                    old_position.line += 1;
                    old_position.character = 0;
                    new_position.line += 1;
                    new_position.character = 0;
                } else {
                    old_position.character += 1;
                    new_position.character += 1;
                }
            }

            let start = new_position;

            for char in edit.replacement.graphemes(true) {
                if char == "\n" {
                    new_position.line += 1;
                    new_position.character = 0;
                } else {
                    new_position.character += 1;
                }
            }

            let end = new_position;

            result.push(EditorTextOp{range: Range{start, end}, replacement: edit.replacement.clone()});

            old_position = edit.range.end;
        }
        result
    }
}

fn position_from_kak_selection_desc(desc: &str) -> anyhow::Result<Position> {
    let (line, character) = desc.split_once(".").ok_or(anyhow!("invalid position"))?;
    Ok(Position {
        line: line.parse::<usize>()? - 1,
        character: character.parse::<usize>()? - 1,
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
                _ => Some(Err(anyhow!("unknown message: {message}"))),
            },
            Some(Err(e)) => Some(Err(e.into())),
            None => None,
        }
    }
}

fn listen_to_editor_messages(socket_to_daemon: impl Write) -> anyhow::Result<()> {
    let mut socket_to_daemon = std::io::LineWriter::new(socket_to_daemon);
    let fifo_path = "/tmp/ethersync-kak-fifo";
    let mut prev_content = "".to_string();
    let mut next_id = 0;
    for message in EditorMessages::new(fifo_path.to_string())? {
        match message? {
            MessageFromEditor::BufferChanged {
                file_path,
                new_content,
            } => {
                let delta = EditorTextDelta::from_diff(&prev_content, &new_content);
                for edit in delta.sequential_ops(&prev_content) {
                    let message = serde_json::to_string(&JSONRPCFromEditor::Request {
                        jsonrpc: JSONRPCVersionTag,
                        id: next_id,
                        payload: EditorProtocolMessageFromEditor::Edit {
                            delta: EditorTextDelta(vec![edit]),
                            uri: "file://".to_owned() + &file_path,
                            revision: 0,
                        },
                    })?;
                    writeln!(socket_to_daemon, "{}", message)?;
                    next_id += 1;
                }
                prev_content = new_content;
            }
            MessageFromEditor::BufferCreated { file_path } => {
                let message = serde_json::to_string(&JSONRPCFromEditor::Request {
                    jsonrpc: JSONRPCVersionTag,
                    id: next_id,
                    payload: EditorProtocolMessageFromEditor::Open {
                        uri: "file://".to_owned() + &file_path,
                    },
                })?;
                println!("sending {}", message);
                writeln!(socket_to_daemon, "{}", message)?;
                next_id += 1;
            }
            MessageFromEditor::CursorMoved { file_path, cursors } => {
                let message = serde_json::to_string(&JSONRPCFromEditor::Request {
                    jsonrpc: JSONRPCVersionTag,
                    id: next_id,

                    payload: EditorProtocolMessageFromEditor::Cursor {
                        uri: "file://".to_owned() + &file_path,
                        ranges: cursors,
                    },
                })?;
                println!("sending {message}");
                writeln!(socket_to_daemon, "{}", message)?;
                next_id += 1;
            }
        };
    }
    Ok(())
}

fn listen_to_daemon_messages(stream: impl Read) -> anyhow::Result<()> {
    let stream = BufReader::new(stream);
    for line in stream.lines() {
        let line = line.unwrap();
        let message: serde_json::Result<EditorProtocolMessageToEditor> =
            serde_json::from_str(&line);
        match message {
            Ok(EditorProtocolMessageToEditor::Edit {
                uri,
                revision,
                delta,
            }) => apply_delta(&delta),
            Ok(EditorProtocolMessageToEditor::Cursor {
                userid,
                name,
                uri,
                ranges,
            }) => {
                println!("got cursor message for user {name:?}, ranges: {ranges:?}")
            }
            Err(_) => {
                println!("got some line: {line}")
            }
        }
    }
    Ok(())
}

fn to_kak_range(r: &Range) -> String {
    format!(
        "{}.{},{}.{}",
        r.start.line, r.start.character, r.end.line, r.end.character
    )
}

fn escape_for_editor(s: &str) -> String {
    s.to_string() // TODO
}

fn apply_delta(delta: &EditorTextDelta) {
    let commands = delta
        .0
        .iter()
        .flat_map(|op| {
            [
                format!("select {}", to_kak_range(&op.range)),
                format!(
                    "execute_keys \"{}{}<esc>\"",
                    if op.range.start == op.range.end {
                        "i"
                    } else {
                        "c"
                    },
                    escape_for_editor(&op.replacement)
                ),
            ]
        })
        .join("\n");
    println!("{commands}");
}

fn main() -> anyhow::Result<()> {
    let socket_path = "/tmp/ethersync";
    let stream = UnixStream::connect(socket_path)?;
    let stream_copy = stream.try_clone()?;

    let editor_thread = thread::spawn(|| listen_to_editor_messages(stream));
    let daemon_thread = thread::spawn(|| listen_to_daemon_messages(stream_copy));
    editor_thread.join().expect("couldn't join editor thread")?;
    daemon_thread.join().expect("couldn't join daemon thread")?;
    Ok(())
}
