These are additional remarks found on the original printout that were
not integrated into the "updates" branch.

Remark on this lines:

```
                out     dx,al                   ; actually send the char
```

"If we've just sent an XOFF, we should turn off THRE interrupts.
Otherwise, data will still be sent but ignored?"


```
                mov     al,01h                  ; disable THRE interrupt
```

"Won't this drop stuff currently in THR?"

```
                out    dx,al                    ; Send the character
```

"This sends \"turn on THRE\" for the third time.  Why?"

```
               mov     al,0fh                  ;enable interrupt line
```

"(Why 'f'?  bit 2 is usually 0')"


General remarks for `_unserial`:

"Why don't we disable the interrupts for RCA + THRE here??"

"Why not reset DTR?"  (Or maybe DTIR?  Difficult to make out handwriting.)
