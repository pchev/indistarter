unit pu_setup;

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

uses u_utils, math,
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls;

type

  { Tf_setup }

  Tf_setup = class(TForm)
    autostart: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    BtnNewDevlist: TButton;
    BtnNewConfig: TButton;
    ConfigList: TComboBox;
    Label8: TLabel;
    SelectDevlist: TComboBox;
    localport: TEdit;
    Panel1: TPanel;
    remoteport: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    remotehost: TEdit;
    remoteuser: TEdit;
    PanelRemote: TPanel;
    remote: TCheckBox;
    PanelBot: TPanel;
    serveroptions: TEdit;
    Label2: TLabel;
    stayontop: TCheckBox;
    Label1: TLabel;
    procedure BtnNewDevlistClick(Sender: TObject);
    procedure BtnNewConfigClick(Sender: TObject);
    procedure ConfigListChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure remoteClick(Sender: TObject);
    procedure SelectDevlistChange(Sender: TObject);
  private
    { private declarations }
    FConfigChange: TNotifyEvent;
  public
    { public declarations }
    ConfigDir,config,devlist: string;
    procedure UpdDevices;
    property onConfigChange: TNotifyEvent read FConfigChange write FConfigChange;
  end;

var
  f_setup: Tf_setup;

implementation

{$R *.lfm}

{ Tf_setup }

procedure Tf_setup.FormShow(Sender: TObject);
var fs : TSearchRec;
    i,n : integer;
    buf: string;
begin
  ConfigList.Clear;
  i:=findfirst(slash(ConfigDir)+'*.conf',0,fs);
  while i=0 do begin
    buf:=ExtractFileNameOnly(fs.name);
    n:=ConfigList.Items.Add(buf);
    if buf=config then ConfigList.ItemIndex:=n;
    i:=findnext(fs);
  end;
  findclose(fs);
  if ConfigList.Items.Count=0 then begin
    ConfigList.Items.Add('default');
    ConfigList.ItemIndex:=0;
  end;
  SelectDevlist.Clear;
  i:=findfirst(slash(ConfigDir)+'*.devices',0,fs);
  while i=0 do begin
    buf:=ExtractFileNameOnly(fs.name);
    n:=SelectDevlist.Items.Add(buf);
    if buf=devlist then SelectDevlist.ItemIndex:=n;
    i:=findnext(fs);
  end;
  findclose(fs);
  if SelectDevlist.Items.Count=0 then begin
    SelectDevlist.Items.Add('default');
    SelectDevlist.ItemIndex:=0;
  end;
end;


procedure Tf_setup.UpdDevices;
var i: integer;
begin
  for i:=0 to SelectDevlist.Items.Count-1 do begin
     if SelectDevlist.Items[i]=devlist then begin
        SelectDevlist.ItemIndex:=i;
        break;
     end;
  end;
end;

procedure Tf_setup.BtnNewDevlistClick(Sender: TObject);
var f1:Tform;
    e1:Tedit;
    b1:Tbutton;
    n: integer;
begin
   f1:=Tform.Create(self);
   e1:=Tedit.Create(f1);
   b1:=Tbutton.Create(f1);
   try
   e1.Parent:=f1;
   b1.Parent:=f1;
   e1.Top:=8; e1.Left:=8;
   e1.Width:=350;
   e1.Text:='';
   b1.Width:=65;
   b1.Top:=e1.Top+e1.Height+8; b1.Left:=8;
   b1.Caption:='OK'; b1.ModalResult:=mrOk; b1.Default:=true;
   f1.ClientWidth:=max(e1.Width,b1.Width)+16;
   f1.ClientHeight:=b1.top+b1.Height+8;
   f1.BorderStyle:=bsDialog;
   f1.Caption:='New profile';
   formpos(f1,mouse.CursorPos.X,mouse.CursorPos.Y);
   if f1.ShowModal=mrOK then begin
      devlist:=trim(e1.Text);
      n:=SelectDevlist.Items.Add(devlist);
      SelectDevlist.ItemIndex:=n;
   end;
   finally
     e1.Free; b1.Free; f1.Free;
   end;
end;

procedure Tf_setup.SelectDevlistChange(Sender: TObject);
begin
  devlist:=SelectDevlist.Items[SelectDevlist.ItemIndex];
end;

procedure Tf_setup.remoteClick(Sender: TObject);
begin
  PanelRemote.Visible:=remote.Checked;
  AutoSize:=false;
  AutoSize:=true;
end;

procedure Tf_setup.ConfigListChange(Sender: TObject);
begin
  config:=ConfigList.Items[ConfigList.ItemIndex];
  if assigned(FConfigChange) then FConfigChange(self);
end;

procedure Tf_setup.BtnNewConfigClick(Sender: TObject);
var f1:Tform;
    e1:Tedit;
    b1:Tbutton;
    n: integer;
begin
   f1:=Tform.Create(self);
   e1:=Tedit.Create(f1);
   b1:=Tbutton.Create(f1);
   try
   e1.Parent:=f1;
   b1.Parent:=f1;
   e1.Top:=8; e1.Left:=8;
   e1.Width:=350;
   e1.Text:='';
   b1.Width:=65;
   b1.Top:=e1.Top+e1.Height+8; b1.Left:=8;
   b1.Caption:='OK'; b1.ModalResult:=mrOk; b1.Default:=true;
   f1.ClientWidth:=max(e1.Width,b1.Width)+16;
   f1.ClientHeight:=b1.top+b1.Height+8;
   f1.BorderStyle:=bsDialog;
   f1.Caption:='New configuration';
   formpos(f1,mouse.CursorPos.X,mouse.CursorPos.Y);
   if f1.ShowModal=mrOK then begin
      config:=trim(e1.Text);
      n:=ConfigList.Items.Add(config);
      ConfigList.ItemIndex:=n;
      ConfigListChange(Sender);
   end;
   finally
     e1.Free; b1.Free; f1.Free;
   end;
end;

end.

