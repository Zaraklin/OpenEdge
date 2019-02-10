def var wpattern as char no-undo format "x(60)".
def var string   as char no-undo format "x(30)".
def var x        as int  no-undo.

procedure regex_match external "/c/regex4progress.so":
  
  def input  param wpattern as char.
  def input  param string   as char.
  def return param x        as long.
end procedure.

wpattern = "~\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+~\.[A-Za-z]~{2,6~}~\b".

message wpattern
  view-as alert-box INFO buttons ok.

update wpattern
       string 
       with side-labels.

run regex_match (wpattern, string, output x).
display x.

return.