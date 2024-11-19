declare-option -hidden int es_timestamp -1
define-command -hidden es-nop-with-0 nop
define-command -hidden es-if-changed-since -params 3 -docstring %{
    es-if-changed-since <option_name> <option_value> <commands>
} %{
	echo -debug "called if changed since"
    declare-option -hidden int es_elapsed_time
    set-option buffer es_elapsed_time %val{timestamp}
    set-option -remove buffer es_elapsed_time %arg{2}
    try %{
        evaluate-commands "es-nop-with-%opt{es_elapsed_time}"
    } catch %{
        set-option buffer %arg{1} %val{timestamp}
        echo -debug "has changed"
        evaluate-commands %arg{3}
    }
}

define-command -hidden es-send -params 1.. %{
    echo -end-of-line -to-file /tmp/ethersync-kak-fifo %arg{@}
}

define-command -hidden es-did-change %{
    es-if-changed-since es_timestamp %opt{es_timestamp} %{
        es-send "BufferChanged"
        es-send %val{buffile}
        es-send %val{buf_line_count}
	    write -force /tmp/ethersync-kak-fifo
    }
}

define-command -hidden es-open-file %{
    es-send "BufferCreated"
    es-send %val{buffile}
}

define-command -hidden es-cursor-moved %{
    es-send "CursorMoved"
    es-send %val{buffile}
    es-send %val{selections_char_desc}
}

define-command es-enable -docstring "Enable Ethersync" %{
	hook -group es %arg{1} InsertIdle .* es-did-change
	hook -group es %arg{1} NormalIdle .* es-did-change

	hook -group es %arg{1} InsertIdle .* es-cursor-moved
	hook -group es %arg{1} NormalIdle .* es-cursor-moved

	hook -group es %arg{1} BufCreate .* es-open-file
	es-open-file
}
