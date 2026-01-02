use anyhow::anyhow;
use bytes::{Buf, BytesMut};
use itertools::Itertools;
use log::{debug, info, warn};
use nix::sys::stat;
use nix::unistd;
use serde::{Deserialize, Serialize};
use similar::{DiffOp, TextDiff};
use std::collections::HashMap;
use std::io::{self};
use std::path::Path;
use std::process::Stdio;
use tokio::io::{AsyncRead, AsyncWriteExt};
use tokio::net::unix::{self, pipe};
use tokio_stream::StreamExt;
use tokio_util::codec::Decoder;
use tokio_util::codec::FramedRead;
use tokio_util::codec::LinesCodec;
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
#[serde(untagged)]
enum JSONRPCFromDaemon {
    Call(EditorProtocolMessageToEditor),
    Result(JSONRPCResponse),
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
    let (line, character) = desc
        .split_once(".")
        .ok_or(anyhow!("invalid position: {desc:?}"))?;
    Ok(Position {
        line: line.parse::<usize>()? - 1,
        character: character.parse::<usize>()? - 1,
    })
}

fn range_from_kak_selection_desc(desc: &str) -> anyhow::Result<Range> {
    let (start, end) = desc
        .split_once(",")
        .ok_or(anyhow!("invalid range: {desc:?}"))?;
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
        content: String,
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
    BufferClosed {
        buffer_name: String,
        file_path: String,
    },
    CursorMoved {
        file_path: String,
        cursors: Vec<Range>,
    },
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
    s.replace('<', "<lt>")
}

fn escape_single_quotes(s: &str) -> String {
    s.replace('\'', "''")
}

fn first_invalid_position(doc: &str) -> Position {
    debug!("first_invalid_position called with doc: {doc:?}");
    let mut result = Position {
        line: 0,
        character: 0,
    };
    for char in doc.graphemes(true) {
        if char == "\n" {
            result.line += 1;
            result.character = 0;
        } else {
            result.character += 1;
        }
    }
    result
}

fn maybe_strip_trailing_newline(s: &str) -> &str {
    match s.strip_suffix("\n") {
        Some(prefix) => prefix,
        None => s,
    }
}

fn apply_delta_to_buffer(doc: &str, buffer_name: &str, delta: &EditorTextDelta) -> String {
    debug!("applying delta: {delta:?}");
    let doc_end = first_invalid_position(doc);
    debug!("doc_end: {doc_end:?}");

    let (insertions, appends) = delta
        .0
        .iter()
        .cloned()
        .partition(|op| doc_end.character != 0 || op.range.start < doc_end);

    let mut commands = String::new();
    for op in EditorTextDelta(insertions).sequential_ops(doc) {
        debug!(
            "replacement: {:?}, bytes: {:x?}",
            op.replacement,
            op.replacement.as_bytes()
        );
        commands += &format!(
            "select {}; execute-keys \'{}{}<esc>\';",
            to_kak_range(&op.range),
            if op.range.start == op.range.end {
                "i"
            } else {
                "<s-h>c"
            },
            escape_single_quotes(&escape_keys(&op.replacement)) //TODO: update doc_end
        )
    }

    for op in appends {
        // We are appending to the end of the document

        // Kakoune will implicitly add a newline to the end of the file after
        // appending, so if the replacement already ends with one, we don't need to type it.
        let replacement = maybe_strip_trailing_newline(&op.replacement);

        commands += &format!(
            "execute-keys \'gea{}<esc>\';",
            escape_single_quotes(&escape_keys(replacement))
        );
    }

    let eval_command = format!(
        "evaluate-commands -buffer '{}' \"{}\"\n",
        escape_single_quotes(buffer_name),
        commands
    );
    debug!("executing command: {eval_command}");
    eval_command
}

struct DaemonConnection {
    next_id: usize,
    write_end: unix::OwnedWriteHalf,
    read_end: FramedRead<unix::OwnedReadHalf, LinesCodec>,
}

impl DaemonConnection {
    fn new(socket: tokio::net::UnixStream) -> Self {
        let (read_end, write_end) = socket.into_split();
        let read_end = FramedRead::new(read_end, LinesCodec::new());
        Self {
            read_end,
            write_end,
            next_id: 0,
        }
    }

    async fn send(&mut self, payload: EditorProtocolMessageFromEditor) -> anyhow::Result<()> {
        let mut message = serde_json::to_string(&JSONRPCFromEditor::Request {
            jsonrpc: JSONRPCVersionTag,
            id: self.next_id,
            payload,
        })?;
        debug!("sending {}", message);
        message.push('\n');
        self.write_end.write_all(message.as_bytes()).await?;
        self.write_end.flush().await?;
        self.next_id += 1;
        Ok(())
    }

    async fn next_message(&mut self) -> Option<anyhow::Result<JSONRPCFromDaemon>> {
        match self.read_end.next().await {
            Some(Ok(line)) => Some(serde_json::from_str(&line).map_err(Into::into)),
            Some(Err(e)) => Some(Err(e.into())),
            None => None,
        }
    }
}

struct EditorConnection {
    fifo_path: String,
    read_end: FramedRead<ReopeningReceiver, EditorMessageDecoder>,
}

impl EditorConnection {
    fn new(fifo_path: String) -> anyhow::Result<Self> {
        unistd::mkfifo(Path::new(&fifo_path), stat::Mode::S_IRWXU)?;
        let read_end = FramedRead::new(
            ReopeningReceiver::new(fifo_path.clone())?,
            EditorMessageDecoder,
        );
        Ok(Self {
            fifo_path,
            read_end,
        })
    }

    async fn next_message(&mut self) -> Option<anyhow::Result<MessageFromEditor>> {
        self.read_end.next().await
    }

    async fn execute_commands(session: &str, commands: &str) -> anyhow::Result<()> {
        let mut child = tokio::process::Command::new("kak")
            .args(["-p", session])
            .stdin(Stdio::piped())
            .spawn()?;
        let stdin = child
            .stdin
            .as_mut()
            .ok_or(anyhow!("couldn't open child stdin"))?;
        stdin.write_all(commands.as_bytes()).await?;
        let status = child.wait().await?;
        if !status.success() {
            Err(anyhow!("failed to send commands to kakoune: {commands:?}"))
        } else {
            Ok(())
        }
    }
}

impl Drop for EditorConnection {
    fn drop(&mut self) {
        std::fs::remove_file(&self.fifo_path).unwrap();
    }
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
            match grapheme_iter.next() {
                Some(char) => {
                    new_content += char;
                    if char == "\n" {
                        old_position.line += 1;
                        old_position.character = 0;
                    } else {
                        old_position.character += 1;
                    }
                }
                None => {
                    debug!("reached end of document while applying delta, old pos: {old_position:?}, edit range start: {:?}", edit.range.start);
                    if edit.range.start.line == old_position.line + 1
                        && edit.range.start.character == 0
                        || edit.range.start.line == old_position.line
                            && edit.range.start.character == old_position.character + 1
                    {
                        break;
                    } else {
                        panic!("edit out of range")
                    }
                }
            }
        }

        new_content += &edit.replacement;

        while old_position < edit.range.end {
            match grapheme_iter.next() {
                Some(char) => {
                    if char == "\n" {
                        old_position.line += 1;
                        old_position.character = 0;
                    } else {
                        old_position.character += 1;
                    }
                }
                None => {
                    break;
                }
            }
        }
    }

    new_content.extend(grapheme_iter);

    new_content
}

#[derive(Debug)]
struct BufferState {
    editor_revision: usize,
    daemon_revision: usize,
    buffer_name: String,
    prev_content: String,
}

impl BufferState {
    fn new(buffer_name: String, initial_content: String) -> Self {
        Self {
            editor_revision: 0,
            daemon_revision: 0,
            buffer_name,
            prev_content: initial_content,
        }
    }
}

struct EditorMessageDecoder;

// Like the try! macro for Options, but it returs Ok(None) if expr is None.
macro_rules! try_and_wrap_in_ok {
    ($expr:expr $(,)?) => {
        match $expr {
            Some(val) => val,
            None => {
                return Ok(None);
            }
        }
    };
}

impl Decoder for EditorMessageDecoder {
    type Item = MessageFromEditor;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let str = std::str::from_utf8(src)?;
        log::trace!("got frame for decoding: {str:?}");
        let mut lines_and_offsets = str
            // this instead of ".lines()" because we only want full lines
            .split_inclusive('\n')
            .scan(0, |offset, line| {
                let result = Some((line, *offset));
                *offset += line.len();
                result
            })
            .filter_map(|(line, offset)| line.strip_suffix('\n').map(|line| (line, offset)));

        let (message_type, _) = try_and_wrap_in_ok!(lines_and_offsets.next());

        let message = match message_type {
            "BufferChanged" => {
                let (file_path, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                let (new_content_length, _) = try_and_wrap_in_ok!(lines_and_offsets.next());

                let mut new_content = String::new();
                for _ in 0..new_content_length.parse()? {
                    let (line, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                    new_content.push_str(line);
                    new_content.push('\n');
                }

                MessageFromEditor::BufferChanged {
                    file_path: file_path.to_string(),
                    new_content,
                }
            }
            "BufferCreated" => {
                let (buffer_name, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                let (file_path, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                let (content_length, _) = try_and_wrap_in_ok!(lines_and_offsets.next());

                let mut content = String::new();
                for _ in 0..content_length.parse()? {
                    let (line, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                    content.push_str(line);
                    content.push('\n');
                }

                MessageFromEditor::BufferCreated {
                    buffer_name: buffer_name.to_string(),
                    file_path: file_path.to_string(),
                    initial_content: content,
                }
            }
            "BufferClosed" => {
                let (buffer_name, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                let (file_path, _) = try_and_wrap_in_ok!(lines_and_offsets.next());

                MessageFromEditor::BufferClosed {
                    buffer_name: buffer_name.to_string(),
                    file_path: file_path.to_string(),
                }
            }
            "SessionStarted" => {
                let (session_name, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                MessageFromEditor::SessionStarted {
                    session_name: session_name.to_string(),
                }
            }
            "CursorMoved" => {
                let (file_path, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                let (cursors, _) = try_and_wrap_in_ok!(lines_and_offsets.next());
                MessageFromEditor::CursorMoved {
                    file_path: file_path.to_string(),
                    cursors: ranges_from_kak_selection_desc(cursors)?,
                }
            }
            _ => return Err(anyhow!("unknown message type: {message_type}")),
        };

        match lines_and_offsets.next() {
            Some((_, offset)) => src.advance(offset),
            None => src.advance(src.len()),
        }
        Ok(Some(message))
    }
}

struct ReopeningReceiver {
    fifo_path: String,
    reciever: pipe::Receiver,
}

impl ReopeningReceiver {
    fn new(fifo_path: String) -> io::Result<Self> {
        let reciever = pipe::OpenOptions::new().open_receiver(&fifo_path)?;
        Ok(Self {
            fifo_path,
            reciever,
        })
    }

    fn reopen_pipe(&mut self) -> io::Result<()> {
        self.reciever = pipe::OpenOptions::new().open_receiver(&self.fifo_path)?;
        Ok(())
    }
}

impl AsyncRead for ReopeningReceiver {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<io::Result<()>> {
        let len_before = buf.filled().len();
        // this is a bit sketchy and i don't really understand it
        let reciever = unsafe { self.as_mut().map_unchecked_mut(|this| &mut this.reciever) };
        match reciever.poll_read(cx, buf) {
            std::task::Poll::Ready(Ok(())) => {
                let len_after = buf.filled().len();
                let bytes_read = len_after - len_before;
                if bytes_read == 0 {
                    log::trace!("reopening pipe");
                    match self.reopen_pipe() {
                        Ok(_) => self.poll_read(cx, buf),
                        Err(e) => std::task::Poll::Ready(Err(e)),
                    }
                } else {
                    std::task::Poll::Ready(Ok(()))
                }
            }
            std::task::Poll::Ready(Err(e)) => {
                eprintln!("got error: {e:?}");
                std::task::Poll::Ready(Err(e))
            }
            std::task::Poll::Pending => std::task::Poll::Pending,
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    let socket_path = ".teamtype/socket";
    let stream = tokio::net::UnixStream::connect(socket_path).await?;

    let mut daemon_connection = DaemonConnection::new(stream);

    let fifo_path = "/tmp/teamtype-kak-fifo";
    let mut editor_connection = EditorConnection::new(fifo_path.to_string())?;

    let mut kak_session = "".to_string();

    let mut buffer_states: HashMap<String, BufferState> = HashMap::new();

    loop {
        tokio::select! {
            message = editor_connection.next_message() => {
                log::trace!("{message:?}");
                match message {
                   Some(Ok(MessageFromEditor::BufferChanged {
                        file_path,
                        new_content,
                    })) => {
                        let buffer_state = match buffer_states.get_mut(&file_path) {
                            Some(state) => state,
                            None => {
                                warn!("received change for unknown buffer: {}", file_path);
                                continue;
                            }
                        };

                        let delta = EditorTextDelta::from_diff(&buffer_state.prev_content, &new_content);
                        debug!("delta: {delta:?}");
                        for edit in delta.sequential_ops(&buffer_state.prev_content) {
                            daemon_connection.send(EditorProtocolMessageFromEditor::Edit {
                                delta: EditorTextDelta(vec![edit]),
                                uri: "file://".to_owned() + &file_path,
                                revision: buffer_state.daemon_revision,
                            }).await?;
                            buffer_state.editor_revision += 1;
                        }
                        buffer_state.prev_content = new_content;
                    }
                   Some(Ok(MessageFromEditor::BufferCreated {
                        file_path,
                        buffer_name,
                        initial_content,
                    })) => {
                        daemon_connection.send(EditorProtocolMessageFromEditor::Open {
                            uri: "file://".to_owned() + &file_path,
                            content: initial_content.clone(),
                        }).await?;

                        buffer_states.insert(
                            file_path.clone(),
                            BufferState::new(buffer_name, initial_content)
                        );
                    }
                    Some(Ok(
                        MessageFromEditor::BufferClosed { buffer_name, file_path }
                    )) => {
                        daemon_connection.send(
                            EditorProtocolMessageFromEditor::Close{uri: "file://".to_owned() + &file_path}
                        ).await?;
                        buffer_states.remove(&file_path);
                    }
                   Some(Ok(MessageFromEditor::SessionStarted { session_name })) => {
                        kak_session = session_name;
                    }
                   Some(Ok(MessageFromEditor::CursorMoved { file_path, cursors })) => {
                        daemon_connection.send(EditorProtocolMessageFromEditor::Cursor {
                            uri: "file://".to_owned() + &file_path,
                            ranges: cursors,
                        }).await?;
                    }
                    Some(Err(e)) => {
                        log::error!("error while reading message from editor: {e}");
                    }
                    None => {
                        log::error!("lost connection to editor");
                        break;
                    }
                }
            }
            message = daemon_connection.next_message() => {
                log::trace!("{message:?}");
                match message {
                    Some(Ok(JSONRPCFromDaemon::Call(EditorProtocolMessageToEditor::Edit {
                        uri,
                        revision,
                        delta,
                    }))) => {
                        let file_path = match uri.strip_prefix("file://") {
                            Some(path) => path,
                            None => {
                                warn!("invalid uri: {}", uri);
                                continue;
                            }
                        };

                        let buffer_state = match buffer_states.get_mut(file_path) {
                            Some(state) => state,
                            None => {
                                warn!("received edit for unknown buffer: {}", file_path);
                                continue;
                            }
                        };

                        if revision != buffer_state.editor_revision {
                            debug!("we've already edited the document further since this revision, skipping");
                            continue;
                        }

                        buffer_state.daemon_revision += 1;

                        EditorConnection::execute_commands(&kak_session, &apply_delta_to_buffer(
                            &buffer_state.prev_content,
                            &buffer_state.buffer_name,
                            &delta
                        )).await?;
                        buffer_state.prev_content = apply_delta_to_string(&buffer_state.prev_content, &delta);
                    }
                    Some(Ok(JSONRPCFromDaemon::Call(EditorProtocolMessageToEditor::Cursor {
                        userid,
                        name,
                        uri,
                        ranges,
                    }))) => {
                        info!("got cursor message for user {name:?}, ranges: {ranges:?}")
                    }
                    Some(Ok(JSONRPCFromDaemon::Result(JSONRPCResponse::RequestSuccess { id, result }))) => {
                        log::debug!("request {id} succeeded, result: {result}");
                    }
                    Some(Ok(JSONRPCFromDaemon::Result(JSONRPCResponse::RequestError {id, error }))) => {
                        Err(anyhow!("request {id:?} failed: {error:?}"))?;
                    }
                    Some(Err(e)) => {
                        Err(e)?;
                    }
                    None => {
                        log::error!("lost connection to daemon");
                        break;
                    }
                }
            }
        }
    }
    Ok(())
}
