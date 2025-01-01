use anyhow::anyhow;
use itertools::Itertools;
use log::{debug, error, info, trace};
use serde::{Deserialize, Serialize};
use similar::{DiffOp, TextDiff};
use std::io::LineWriter;
use std::process::{Command, Stdio};
use std::sync::mpsc::Sender;
use std::{
    fs::File,
    io::{BufRead, BufReader, Read, Write},
    os::unix::net::UnixStream,
    sync::mpsc,
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

    fn sequential_ops(self, document: &str) -> Vec<EditorTextOp> {
        if self.0.len() == 1 {
            return self.0;
        }

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
            // advance both old and new pointers until old pointer is at the
            // beginning of this edit
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

            let line_diff = new_position.line as i32 - old_position.line as i32;
            let char_diff = new_position.character as i32 - old_position.character as i32;
            result.push(EditorTextOp {
                range: Range {
                    start: new_position,
                    end: Position {
                        line: (edit.range.end.line as i32 + line_diff) as usize,
                        character: (edit.range.end.character as i32 + char_diff) as usize,
                    },
                },
                replacement: edit.replacement.clone(),
            });

            // advance only new pointer through the replacement
            for char in edit.replacement.graphemes(true) {
                if char == "\n" {
                    new_position.line += 1;
                    new_position.character = 0;
                } else {
                    new_position.character += 1;
                }
            }

            // advance old pointer to the end of the range in the original doc
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
    SessionStarted {
        session_name: String,
    },
    BufferChanged {
        file_path: String,
        new_content: String,
    },
    BufferCreated {
        buffer_name: String,
        file_path: String,
        initial_content: String,
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
                "BufferCreated" => {
                    fn read_buffer_created_message(
                        fifo_lines: &mut FifoLines,
                    ) -> anyhow::Result<MessageFromEditor> {
                        let buffer_name = fifo_lines
                            .next()
                            .ok_or(anyhow!("invalid CursorMoved message"))??;
                        let file_path = fifo_lines
                            .next()
                            .ok_or(anyhow!("invalid CursorMoved message"))??;
                        let num_lines: usize = fifo_lines
                            .next()
                            .ok_or(anyhow!("invalid BufferChanged message"))??
                            .parse()?;
                        let content: Vec<String> = fifo_lines.take(num_lines).try_collect()?;
                        if content.len() < num_lines {
                            return Err(anyhow!("invalid BufferChanged message"));
                        }
                        let content = content.join("\n");
                        Ok(MessageFromEditor::BufferCreated {
                            buffer_name,
                            file_path,
                            initial_content: content,
                        })
                    }
                    Some(read_buffer_created_message(&mut self.fifo_lines))
                }
                "SessionStarted" => match self.fifo_lines.next() {
                    Some(Ok(session_name)) => {
                        Some(Ok(MessageFromEditor::SessionStarted { session_name }))
                    }
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

fn listen_to_editor_messages(sender: Sender<Message>) -> anyhow::Result<()> {
    let fifo_path = "/tmp/ethersync-kak-fifo";
    for message in EditorMessages::new(fifo_path.to_string())? {
        sender.send(Message::FromEditor(message?))?;
    }
    Ok(())
}

fn listen_to_daemon_messages(stream: impl Read, sender: Sender<Message>) -> anyhow::Result<()> {
    let stream = BufReader::new(stream);
    for line in stream.lines() {
        let line = line.unwrap();
        let message = serde_json::from_str(&line);
        match message {
            Ok(message) => {
                sender.send(Message::FromDaemon(message))?;
            }
            Err(_) => {
                let message: Result<JSONRPCResponse, serde_json::Error> =
                    serde_json::from_str(&line);
                match message {
                    Ok(JSONRPCResponse::RequestSuccess { id, result }) => {
                        trace!("request {id}: {result}")
                    }
                    Ok(JSONRPCResponse::RequestError { id, error }) => {
                        error!("daemon returned error for message {id:?}: {error:?}")
                    }
                    Err(e) => error!("couldn't parse response from daemon: {e}"),
                }
            }
        }
    }
    Ok(())
}

fn to_kak_range(r: &Range) -> String {
    format!(
        "{}.{},{}.{}",
        r.start.line + 1,
        r.start.character + 1,
        r.end.line + 1,
        r.end.character + 1,
    )
}

fn escape_keys(s: &str) -> String {
    s.to_string() // TODO
}

fn escape_single_quotes(s: &str) -> String {
    s.to_string() // TODO
}

fn apply_delta_to_buffer(
    doc: &str,
    session_name: &str,
    buffer_name: &str,
    delta: EditorTextDelta,
) -> anyhow::Result<()> {
    debug!("applying delta: {delta:?}");
    let commands = delta
        .sequential_ops(doc)
        .iter()
        .flat_map(|op| {
            debug!(
                "replacement: {:?}, bytes: {:x?}",
                op.replacement,
                op.replacement.as_bytes()
            );
            [
                format!("select {}", to_kak_range(&op.range)),
                format!(
                    "execute-keys \'{}{}<esc>\'",
                    if op.range.start == op.range.end {
                        "i"
                    } else {
                        "<s-h>c"
                    },
                    &op.replacement
                ),
            ]
        })
        .join("; ");
    let eval_command = format!(
        "evaluate-commands -buffer '{}' \"{}\"\n",
        escape_single_quotes(buffer_name),
        escape_single_quotes(&escape_keys(&commands))
    );
    debug!("executing command: {eval_command}");
    send_commands_to_kakoune(session_name, &eval_command)?;
    Ok(())
}

enum Message {
    FromEditor(MessageFromEditor),
    FromDaemon(EditorProtocolMessageToEditor),
}

struct DaemonConnection {
    next_id: usize,
    writer: LineWriter<UnixStream>,
}

impl DaemonConnection {
    fn new(socket: UnixStream) -> Self {
        Self {
            writer: LineWriter::new(socket),
            next_id: 0,
        }
    }

    fn send(&mut self, payload: EditorProtocolMessageFromEditor) -> anyhow::Result<()> {
        let message = serde_json::to_string(&JSONRPCFromEditor::Request {
            jsonrpc: JSONRPCVersionTag,
            id: self.next_id,
            payload,
        })?;
        debug!("sending {}", message);
        writeln!(self.writer, "{}", message)?;
        self.next_id += 1;
        Ok(())
    }
}

fn send_commands_to_kakoune(session: &str, commands: &str) -> anyhow::Result<()> {
    let mut child = Command::new("kak")
        .args(["-p", session])
        .stdin(Stdio::piped())
        .spawn()?;
    let stdin = child
        .stdin
        .as_mut()
        .ok_or(anyhow!("couldn't open child stdin"))?;
    stdin.write_all(commands.as_bytes())?;
    child.wait()?;
    Ok(())
}

fn apply_delta_to_string(prev_content: &str, delta: &EditorTextDelta) -> String {
    let mut new_content = String::new();
    let mut grapheme_iter = prev_content.graphemes(true);
    let mut old_position = Position {
        line: 0,
        character: 0,
    };
    for edit in &delta.0 {
        while old_position < edit.range.start {
            let char = grapheme_iter.next().expect("edit out of range");
            new_content += char;
            if char == "\n" {
                old_position.line += 1;
                old_position.character = 0;
            } else {
                old_position.character += 1;
            }
        }

        new_content += &edit.replacement;

        while old_position < edit.range.end {
            let char = grapheme_iter.next();
            if char == Some("\n") {
                old_position.line += 1;
                old_position.character = 0;
            } else {
                old_position.character += 1;
            }
        }
    }

    new_content.extend(grapheme_iter);

    new_content
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let socket_path = "/tmp/ethersync";
    let stream = UnixStream::connect(socket_path)?;
    let stream_copy_2 = stream.try_clone()?;

    let (sender, reciever) = mpsc::channel();
    let sender_copy = sender.clone();

    thread::spawn(|| listen_to_editor_messages(sender));
    thread::spawn(|| listen_to_daemon_messages(stream_copy_2, sender_copy));

    let mut daemon_connection = DaemonConnection::new(stream);
    let mut prev_content = "".to_string();
    let mut editor_revision = 0;
    let mut daemon_revision = 0;

    let mut kak_session = "".to_string();

    let mut current_buffer_name = "".to_string();

    loop {
        match reciever.recv()? {
            Message::FromEditor(MessageFromEditor::BufferChanged {
                file_path,
                new_content,
            }) => {
                let delta = EditorTextDelta::from_diff(&prev_content, &new_content);
                debug!("delta: {delta:?}");
                for edit in delta.sequential_ops(&prev_content) {
                    daemon_connection.send(EditorProtocolMessageFromEditor::Edit {
                        delta: EditorTextDelta(vec![edit]),
                        uri: "file://".to_owned() + &file_path,
                        revision: daemon_revision,
                    })?;
                    editor_revision += 1;
                }
                prev_content = new_content;
            }
            Message::FromEditor(MessageFromEditor::BufferCreated {
                file_path,
                buffer_name,
                initial_content,
            }) => {
                prev_content = initial_content;
                current_buffer_name = buffer_name;
                daemon_connection.send(EditorProtocolMessageFromEditor::Open {
                    uri: "file://".to_owned() + &file_path,
                })?;
            }
            Message::FromEditor(MessageFromEditor::SessionStarted { session_name }) => {
                kak_session = session_name;
            }
            Message::FromEditor(MessageFromEditor::CursorMoved { file_path, cursors }) => {
                daemon_connection.send(EditorProtocolMessageFromEditor::Cursor {
                    uri: "file://".to_owned() + &file_path,
                    ranges: cursors,
                })?;
            }

            Message::FromDaemon(EditorProtocolMessageToEditor::Edit {
                uri,
                revision,
                delta,
            }) => {
                if revision != editor_revision {
                    continue;
                }
                prev_content = apply_delta_to_string(&prev_content, &delta);
                apply_delta_to_buffer(&prev_content, &kak_session, &current_buffer_name, delta)?;
                daemon_revision += 1;
            }
            Message::FromDaemon(EditorProtocolMessageToEditor::Cursor {
                userid,
                name,
                uri,
                ranges,
            }) => {
                info!("got cursor message for user {name:?}, ranges: {ranges:?}")
            }
        }
    }
}
