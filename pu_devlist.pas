unit pu_devlist;

{$mode objfpc}{$H+}

interface

uses u_utils, XMLRead, DOM, Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type

  Tdevicenode = class(TObject)
     GroupName,DevLbl,DrvName,Drv,DrvVer : string;
     node: TTreeNode;
  end;

  { Tf_devlist }

  Tf_devlist = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    procedure ProcessDevices;
    procedure processGroup(Node: TDOMNode);
  public
    { public declarations }
  end;

var
  f_devlist: Tf_devlist;

implementation

{$R *.lfm}

{ Tf_devlist }

procedure Tf_devlist.FormCreate(Sender: TObject);
begin
  ProcessDevices;
end;

procedure Tf_devlist.FormDestroy(Sender: TObject);
var i: integer;
begin
  for i:=0 to TreeView1.Items.Count-1 do begin
     if TreeView1.Items[i].Data<>nil then Tdevicenode(TreeView1.Items[i].Data).Free;
  end;
end;

procedure Tf_devlist.ProcessDevices;
var i: integer;
    drvdata,buf,fn,fdir : string;
    fs : TSearchRec;
    f: textfile;
    s: TStringStream;
    Doc: TXMLDocument;
    Node: TDOMNode;
begin
  if Application.HasOption('d', 'drivers') then begin
    fn:=Application.GetOptionValue('d', 'drivers');
  end
  else
    fn:='';
  if not FileExists(fn) then begin
    {$ifdef darwin}
    // try xIndi first
    fn:='/Applications/INDI Server.app/Contents/Resources/share/indi/drivers.xml';
    {$endif}
    if not FileExists(fn) then begin
    fn:='/usr/share/indi/drivers.xml';
      if not FileExists(fn) then begin
       fn:='/usr/local/share/indi/drivers.xml';
       if not FileExists(fn) then begin
         ShowMessage('Cannot find INDI drivers.xml file. Please give the file location using the --drivers= option.');
         halt(1);
       end;
      end;
    end;
  end;
  fdir:=ExtractFilePath(fn);
  drvdata:='';
  i:=findfirst(slash(fdir)+'*.xml',0,fs);
  while i=0 do begin
    fn:=slash(fdir)+fs.name;
    if FileExists(fn) then begin
      AssignFile(f,fn);
      reset(f);
      repeat
        readln(f,buf);
        drvdata:=drvdata+buf;
      until eof(f);
      CloseFile(f);
    end;
    i:=findnext(fs);
  end;
  FindClose(fs);
  drvdata:='<INDIDEV>'+drvdata+'</INDIDEV>';
  s:=TStringStream.Create(drvdata);
  ReadXMLFile(Doc,s);
  Node:=Doc.DocumentElement.FirstChild;
  while Node<>nil do begin
    if Node.NodeName='devGroup' then processGroup(Node);
    Node:=Node.NextSibling;
  end;
  s.Free;
  Doc.Free;
end;

procedure Tf_devlist.processGroup(Node: TDOMNode);
var Ndev,Nprop: TDOMNode;
    GroupName,buf: string;
    TreeGroup: TTreeNode;
    dev: Tdevicenode;
begin
  GroupName:=GetNodeValue(GetAttrib(Node,'group'));
  TreeGroup:=TreeView1.Items.FindNodeWithText(GroupName);
  if TreeGroup=nil then TreeGroup:=TreeView1.Items.AddFirst(nil,GroupName);
  Ndev:=Node.FirstChild;
  while Ndev<>nil do begin
    if Ndev.NodeName='device' then begin
      dev:=Tdevicenode.Create;
      dev.DevLbl:=GetNodeValue(GetAttrib(Ndev,'label'));
      dev.GroupName:=GroupName;
      Nprop:=Ndev.FirstChild;
      while Nprop<>nil do begin
        if Nprop.NodeName='driver' then begin
           dev.DrvName:=GetNodeValue(GetAttrib(Nprop,'name'));
           dev.Drv:=GetChildValue(Nprop);
        end
        else if Nprop.NodeName='version' then begin
          dev.DrvVer:=GetChildValue(Nprop);
        end;
        Nprop:=Nprop.NextSibling;
      end;
      buf:=dev.DevLbl;
      if buf='' then buf:=dev.DrvName;
      if buf='' then buf:=dev.Drv;
      if buf <>'' then TreeView1.Items.AddChildObject(TreeGroup,buf,dev)
                  else dev.Free;
    end;
    Ndev:=Ndev.NextSibling;
  end;
end;

end.

