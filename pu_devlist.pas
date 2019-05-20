unit pu_devlist;

{$mode objfpc}{$H+}

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


interface

uses u_utils, XMLRead, DOM, Classes, SysUtils, FileUtil, UScaleDPI,
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
    CustomCheckBox: TCheckBox;
    CustomDriver: TEdit;
    CustomHost: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    CustomPanel: TPanel;
    TreeView1: TTreeView;
    procedure CustomCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  ScaleDPI(Self);
end;

procedure Tf_devlist.CustomCheckBoxChange(Sender: TObject);
begin
 CustomPanel.Visible:=CustomCheckBox.Checked;
 TreeView1.Visible:= not CustomPanel.Visible;
end;

procedure Tf_devlist.FormDestroy(Sender: TObject);
var i: integer;
begin
  for i:=0 to TreeView1.Items.Count-1 do begin
     if TreeView1.Items[i].Data<>nil then Tdevicenode(TreeView1.Items[i].Data).Free;
  end;
end;

procedure Tf_devlist.FormShow(Sender: TObject);
begin
  CustomPanel.Visible:=CustomCheckBox.Checked;
  TreeView1.Visible:= not CustomPanel.Visible;
  ProcessDevices;
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
  {$ifdef darwin}
  fn:=slash(appdir)+'Resources/DriverSupport/drivers.xml';
  {$else}
  if Application.HasOption('d', 'drivers') then begin
    fn:=Application.GetOptionValue('d', 'drivers');
  end
  else
    fn:='';
  if not FileExists(fn) then begin
    if Bindir<>'' then
       fn:=slash(Bindir)+'..'+ '/share/indi/drivers.xml';
    if not FileExists(fn) then begin
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
  end;
  {$endif}
  fdir:=ExtractFilePath(fn);
  i:=findfirst(slash(fdir)+'*.xml',0,fs);
  TreeView1.Items.Clear;
  TreeView1.BeginUpdate;
  while i=0 do begin
    fn:=slash(fdir)+fs.name;
    if FileExists(fn) then begin
      AssignFile(f,fn);
      reset(f);
      drvdata:='';
      repeat
        readln(f,buf);
        drvdata:=drvdata+buf;
      until eof(f);
      CloseFile(f);
      if pos('?xml',drvdata)=0 then begin     // old format (changed in INDI rev 2715)
        drvdata:='<INDIDEV>'+drvdata+'</INDIDEV>';
      end;
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
    i:=findnext(fs);
  end;
  FindClose(fs);
  TreeView1.SortType:=stText;
  TreeView1.EndUpdate;
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

