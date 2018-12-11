.model small
.stack 100h

.data
    ifn db 13 dup (0)
    ofn db 13 dup (0)
    ifh dw 0
    ofh dw 1
    current_symbol db ?
    in_buff db 512 dup (?)
    in_buff_end dw ?
    in_buff_length dw ?
    out_buff db 512 dup (?)
    out_buff_i dw 0
    word_buff db 75 dup (?)
    word_length db 0
    par_i db 0
    ;Strings & error msgs
    help_msg db "Usage: parmaker {input file name} {output file name}", 0Dh, 0Ah, 9, "/?: show this help message", 0Dh, 0Ah, 9
    help_cont_msg db "Defaults if omitted: stdin and stdout$"
    open_if_err_msg db '!Error opening input file. Will default to reading from stdin', 0Dh, 0Ah, 24h
    create_of_err_msg db '!Error creating output file. Will default to writing to stdout', 0Dh, 0Ah, 24h
    close_if_err_msg db '!Error closing input file', 0Dh, 0Ah, 24h
    close_of_err_msg db '!Error closing output file', 0Dh, 0Ah, 24h
    empty_file_msg db 'Input file is empty', 0Dh, 0Ah, 24h
   
.code
start:
    mov dx, @data
    mov ds, dx
    xor dx, dx

read_pars:
    xor ch, ch
    mov cl, byte ptr [es:80h]
    cmp cl, 0
    je do_help
    dec cl
    mov si, 82h
    cmp word ptr [es:si], "?/"
    je do_help

    lea di, ifn
    read_ifn:
        cmp byte ptr [es:si], " "
        je end_read_ifn
        cmp byte ptr [es:si], 0
        je end_read_ifn
        mov dl, [es:si]
        mov [di], dl
        inc si
        inc di
    loop read_ifn
    end_read_ifn:

    cmp cl, 0
    je open_if
    
    inc si
    dec cl
    lea di, ofn
    read_ofn:
        cmp byte ptr [es:si], 0
        je open_if
        mov dl, [es:si]
        mov [di], dl
        inc si
        inc di
    loop read_ofn

    mov dx, ds
    mov es, dx
    xor dx, dx

    jmp open_if

;Help message
do_help:
    mov ah, 09h
    lea dx, help_msg ;prints both help_msg and help_cont_msg
    int 21h
    mov ax, 4C00h
    int 21h

;Open input file
open_if:
    mov ax, 3D00h
    lea dx, ifn
    int 21h
    jc open_if_error ;Defaults input file to stdin
    mov ifh, ax
    jmp create_of

;Input file error    
open_if_error:
    mov ah, 09h
    lea dx, open_if_err_msg
    int 21h

;Create output file
create_of:
    mov ah, 3Ch
    mov cx, 0
    lea dx, ofn
    int 21h
    jc create_of_error ;Defaults output file to stdout
    mov ofh, ax
    jmp main_logic
        
;Output file error
create_of_error:
    mov ah, 09h
    lea dx, create_of_err_msg
    int 21h
    
main_logic:

    ;Prep for mainloop (read and load print buff address (di))
    call Read
    cmp in_buff_length, 0
    ja not_empty_file
    mov ah, 09h
    lea dx, empty_file_msg
    int 21h
    jmp close_if
    not_empty_file:
    lea di, out_buff

    main_loop:
        mov dl, byte ptr [si]
        mov current_symbol, dl

        ;Deal with spaces
        cmp dl, " "
        jne not_space
        cmp word_length, 0
        je cont_main_loop
        call CopyWord
        call PushSpace
        jmp cont_main_loop
        not_space:

        ;Deal with newlines
        cmp dl, 10
        jne not_newline
        cmp word_length, 0
        je cont_main_loop
        call CopyWord
        call PushNewline
        call PushNewline
        jmp cont_main_loop
        not_newline:

        ;Skip input carriage returns
        cmp dl, 13
        je cont_main_loop

        cmp word_length, 0
        ja already_word

        ;Word slicing from input buffer
        begin_word:
            mov word_length, 0
            lea bx, word_buff

        already_word:
            mov [bx], dl
            inc bx
            inc word_length
            inc par_i

        ;Pushing newline after reaching 74 symbols in a line (1 reserved for space)
        cmp par_i, 74
        jb cont_main_loop
        call PushNewline
        
        ;Increase input buffer iterator (si) address and check for read and print req's
        cont_main_loop:
            inc si
            cmp si, in_buff_end
            jb skip_read
            cmp in_buff_length, 512
            jb exit_main_loop
            call Read
            cmp in_buff_length, 0
            je exit_main_loop
            skip_read:
            cmp out_buff_i, 512
            jb skip_print
            call Print
            skip_print:
    jmp main_loop
    exit_main_loop:
    ;Flush buffers after exit
    call CopyWord
    call Print

;Close input file
close_if:
    ;If STDIN, skip closing
    cmp ifh, 0
    je close_of
    mov ah, 3Eh
    mov bx, ifh
    int 21h
    jnc close_of
    mov ah, 09h
    lea dx, close_if_err_msg
    int 21h
    
;Close output file
close_of:
    ;If STDOUT, skip closing
    cmp ofh, 1
    je exit
    mov ah, 3Eh
    mov bx, ofh
    int 21h
    jnc exit
    mov ah, 09h
    lea dx, close_of_err_msg
    int 21h
    
exit:
    mov ax, 4C00h
    int 21h

procs:
;Read 512 characters from input file to input buffer, reset si to beginning
proc Read
    push ax
    push bx
    push cx
    push dx
    
    mov ah, 3Fh
    mov bx, ifh
    mov cx, 512
    lea dx, in_buff
    int 21h

    lea si, in_buff
    mov in_buff_end, si
    add in_buff_end, ax
    mov in_buff_length, ax
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp Read

;Print current amount of characters from output buffer to output file, reset di to beginning
proc Print
    push ax
    push bx
    push cx
    push dx
    
    mov ah, 40h
    mov bx, ofh
    mov cx, out_buff_i
    lea dx, out_buff
    int 21h

    mov out_buff_i, 0
    lea di, out_buff

    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp Print

;Copy current amount of characters from word buffer to output buffer, reset bx to beginning
proc CopyWord
    cmp word_length, 0
    je no_need
    push ax
    push cx
    push si
    xor ch, ch
    xor ax, ax

    ;Calculate output buffer index as if the word was copied (to check if word fits)
    mov ax, out_buff_i
    add al, word_length
    ;We did AL, it might've overflown, so overflow jump required to deal with ah
    jno no_carry
    inc ah
    no_carry:
    ;Flush output buffer if won't fit
    cmp ax, 512
    jb skip_copyword_print
    call Print
    skip_copyword_print:

    mov cl, word_length
    add out_buff_i, cx
    lea si, word_buff
    rep movsb

    lea bx, word_buff
    mov word_length, 0

    pop si
    pop cx
    pop ax
    no_need:
    ret
endp CopyWord

;Copy two symbols (\r\n) to output buffer
proc PushNewline
    ;Flush output buffer if won't fit
    cmp out_buff_i, 510
    jb skip_newline_print
    call Print
    skip_newline_print:

    mov byte ptr [di], 13
    inc di
    mov byte ptr [di], 10
    inc di
    add out_buff_i, 2
    mov par_i, 0

    ret
endp PushNewline

;Copy one symbol (" ") to output buffer
proc PushSpace
    ;Flush output buffer if won't fit (shouldn't ever happen)
    cmp out_buff_i, 511
    jb skip_space_print
    call Print
    skip_space_print:

    mov byte ptr [di], " "
    inc di
    inc out_buff_i
    inc par_i

    ret
endp PushSpace

end start