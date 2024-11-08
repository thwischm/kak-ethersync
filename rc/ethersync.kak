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

define-command -hidden es-did-change -docstring "Notify language server about buffer change" %{
    es-if-changed-since es_timestamp %opt{es_timestamp} %{
	    write -force /tmp/ethersync-kak-fifo
    }
}

define-command es-enable -docstring "Enable Ethersync" %{
	hook -group es %arg{1} InsertIdle .* es-did-change
	hook -group es %arg{1} NormalIdle .* es-did-change
}
