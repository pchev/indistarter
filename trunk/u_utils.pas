unit u_utils;

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. 

}

{$mode delphi}{$H+}

interface

uses
     {$ifdef mswindows}
       Windows, registry,
     {$endif}
     {$ifdef unix}
       unix,
     {$endif}
     process, dom, Classes, LCLType, FileUtil,
     Math, SysUtils, Forms, Controls, StdCtrls, Graphics;

const
  starter_version='Version alpha 0.0.1';
  starterver = '0.0.1';
  blank=' ';
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  secperday = 3600*24;
  pi2 = 2 * pi;
  rad2deg=180/pi;
  deg2rad=pi/180;

Procedure FormPos(form : Tform; x,y : integer);
Function FormEntry(aOwner:TComponent; lbl,defaultstr:string):string;
function words(str,sep : string; p,n : integer; isep:char=blank) : string;
procedure SplitRec(buf,sep:string; var arg: TStringList);
Procedure SplitCmd(S : String; List : TStringList);
function Slash(nom : string) : string;
Function sgn(x:Double):Double ;
function ExecProcessNoWait(cmd: string):TProcess;
Function ExecProcess(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
procedure Wait(wait:integer=5);
function  Rmod(x,y:Double):Double;
function GetAttrib(node:TDOMNode; attr:string):TDOMNode;
function GetNodeName(node:TDOMNode): string;
function GetNodeValue(node:TDOMNode): string;
function GetChildValue(node:TDOMNode): string;

implementation

Procedure FormPos(form : Tform; x,y : integer);
const margin=60; //minimal distance from screen border
begin
with Form do begin
  if x>margin then left:=x
     else left:=margin;
  if left+width>(Screen.Width-margin) then left:=Screen.Width-width-margin;
  if left<0 then left:=0;
  if y>margin then top:=y
     else top:=margin;
  if top+height>(Screen.height-margin) then top:=Screen.height-height-margin;
  if top<0 then top:=0;
end;
end;

Function FormEntry(aOwner:TComponent; lbl,defaultstr:string):string;
var f: TForm;
    l: Tlabel;
    e: Tedit;
    b: TButton;
begin
  f:=TForm.Create(aOwner);
  l:=TLabel.Create(f);
  e:=TEdit.Create(f);
  b:=TButton.Create(f);
  l.Caption:=lbl;
  l.Parent:=f;
  e.Text:=defaultstr;
  e.Parent:=f;
  b.Caption:='OK';
  b.ModalResult:=mrOK;
  b.Parent:=f;
  f.ChildSizing.ControlsPerLine:=2;
  f.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  f.AutoSize:=true;
  FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
  f.ShowModal;
  if f.ModalResult=mrOK then
    result:=e.Text
  else
    result:=defaultstr;
  f.free;
end;

function words(str,sep : string; p,n : integer; isep:char=blank) : string;
var     i,j : Integer;
begin
result:='';
str:=trim(str);
for i:=1 to p-1 do begin
 j:=pos(isep,str);
 if j=0 then j:=length(str)+1;
 str:=trim(copy(str,j+1,length(str)));
end;
for i:=1 to n do begin
 j:=pos(isep,str);
 if j=0 then j:=length(str)+1;
 result:=result+trim(copy(str,1,j-1))+sep;
 str:=trim(copy(str,j+1,length(str)));
end;
end;

procedure SplitRec(buf,sep:string; var arg: TStringList);
var i,l:integer;
begin
arg.clear;
l:=length(sep);
while pos(sep,buf)<>0 do begin
 for i:=1 to length(buf) do begin
  if copy(buf,i,l) = sep then begin
      arg.add(copy(buf,1,i-1));
      delete(buf,1,i-1+l);
      break;
  end;
 end;
end;
arg.add(buf);
end;

Procedure SplitCmd(S : String; List : TStringList);
  Function GetNextWord : String;
  Const
    WhiteSpace = [' ',#9,#10,#13];
    Literals = ['"',''''];
  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : char;
  begin
    WStart:=1;
    While (WStart<=Length(S)) and (S[WStart] in WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not (S[Wend] in WhiteSpace) or InLiteral) do
      begin
      if S[Wend] in Literals then
        If InLiteral then
          InLiteral:=Not (S[Wend]=LastLiteral)
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          end;
       inc(wend);
       end;
     Result:=Copy(S,WStart,WEnd-WStart);
     if  (Length(Result) > 0)
     and (Result[1] = Result[Length(Result)]) // if 1st char = last char and..
     and (Result[1] in Literals) then // it's one of the literals, then
       Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)
     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);
  end;
Var
  W : String;
begin
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      List.Add(W);
    end;
end;

function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(result),1)<>PathDelim then result:=result+PathDelim;
end;

Function sgn(x:Double):Double ;
begin
// sign function with zero positive
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;

function ExecProcessNoWait(cmd: string):TProcess;
var param: TStringList;
begin
 result:=TProcess.Create(nil);
 param:=TStringList.Create;
 try
   SplitCmd(cmd,param);
   cmd:= param[0];
   param.Delete(0);
   result.Executable:=cmd;
   result.Parameters:=param;
   result.ShowWindow:=swoHIDE;
   result.Execute;
   param.Free;
 except
   on E: Exception do begin
     result:=nil;
     param.Free;
   end;
 end;
end;

Function ExecProcess(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
const READ_BYTES = 2048;
var
  M: TMemoryStream;
  P: TProcess;
  param: TStringList;
  n: LongInt;
  BytesRead: LongInt;
begin
M := TMemoryStream.Create;
P := TProcess.Create(nil);
param:=TStringList.Create;
result:=1;
try
  BytesRead := 0;
  SplitCmd(cmd,param);
  cmd:= param[0];
  param.Delete(0);
  P.Executable:=cmd;
  P.Parameters:=param;
  if ShowConsole then begin
     P.ShowWindow:=swoShowNormal;
     P.StartupOptions:=[suoUseShowWindow];
  end else begin
     P.ShowWindow:=swoHIDE;
  end;
  P.Options := [poUsePipes, poStdErrToOutPut];
  P.Execute;
  while P.Running do begin
    Application.ProcessMessages;
    if P.Output<>nil then begin
      M.SetSize(BytesRead + READ_BYTES);
      n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then inc(BytesRead, n);
    end;
  end;
  result:=P.ExitStatus;
  if (result<>127)and(P.Output<>nil) then repeat
    M.SetSize(BytesRead + READ_BYTES);
    n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until (n<=0)or(P.Output=nil);
  M.SetSize(BytesRead);
  output.LoadFromStream(M);
  P.Free;
  M.Free;
  param.Free;
except
  on E: Exception do begin
    result:=-1;
    output.add(E.Message);
    P.Free;
    M.Free;
    param.Free;
  end;
end;
end;

procedure Wait(wait:integer=5);
var endt: TDateTime;
begin
  endt:=now+wait/secperday;
  while now<endt do begin
    Sleep(100);
    Application.ProcessMessages;
  end;
end;

function  Rmod(x,y:Double):Double;
BEGIN
    Rmod := x - Int(x/y) * y ;
END  ;

function GetAttrib(node:TDOMNode; attr:string):TDOMNode;
begin
  if node=nil then result:=nil
  else result:=node.Attributes.GetNamedItem(DOMString(attr));
end;

function GetNodeName(node:TDOMNode): string;
begin
  if node=nil then result:=''
  else result:=trim(string(node.NodeName));
end;

function GetNodeValue(node:TDOMNode): string;
begin
  if node=nil then result:=''
  else result:=trim(string(node.NodeValue));
end;

function GetChildValue(node:TDOMNode): string;
var cnode: TDOMNode;
begin
  if node=nil then result:=''
  else begin
    cnode:=node.FirstChild;
    if (cnode=nil) then result:=''
    else result:=trim(string(cnode.NodeValue));
  end
end;

end.

