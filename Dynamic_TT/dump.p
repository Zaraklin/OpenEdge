def var tt-handle as handle no-undo.
def var i         as int    no-undo.

create temp-table tt-handle.
tt-handle:add-new-field("id", "integer", 0, "999").
tt-handle:add-new-field("desc", "character", 0, "x(20)").
tt-handle:add-new-field("data", "date", 0, "99/99/999").
tt-handle:temp-table-prepare("tt-din").

do i = 1 to 10.
    create tt-din.
    assign tt-din.id   = i
           tt-din.desc = "dinamycally " + string(i)
           tt-din.data = today.
end.

for each tt-din no-lock:
    disp tt-din.
end.
