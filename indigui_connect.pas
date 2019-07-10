unit indigui_connect;

{$mode objfpc}{$H+}

interface

uses pu_indigui, u_utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type

  { Tf_connect }

  Tf_connect = class(TForm)
    BtnConnect: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuHelp: TMenuItem;
    ServerHost: TEdit;
    ServerPort: TEdit;
    Timer1: TTimer;
    procedure BtnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuHelpClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    autoconnect: boolean;
  public
    procedure Init;
  end;

var
  f_connect: Tf_connect;

implementation

{$R *.lfm}
{$i revision.inc}

{ Tf_connect }

procedure Tf_connect.FormCreate(Sender: TObject);
begin
  autoconnect:=true;
  if Application.HasOption('h', 'host') then begin
    ServerHost.Text:=Application.GetOptionValue('h', 'host');
  end;
  if Application.HasOption('p', 'port') then begin
    ServerPort.Text:=Application.GetOptionValue('p', 'port');
  end;
  if Application.HasOption('n', 'noconnect') then begin
    autoconnect:=false;
  end;
end;

procedure Tf_connect.Init;
begin
   Caption:='INDI Client '+starter_version+'-'+RevisionStr;
   if autoconnect then timer1.Enabled:=true;
end;

procedure Tf_connect.Timer1Timer(Sender: TObject);
begin
  timer1.Enabled:=false;
  BtnConnectClick(Sender);
end;

procedure Tf_connect.BtnConnectClick(Sender: TObject);
begin
  f_indigui:=Tf_indigui.Create(self);
  f_indigui.Caption:='INDI Client '+starter_version+'-'+RevisionStr;
  f_indigui.IndiServer:=ServerHost.Text;
  f_indigui.IndiPort:=ServerPort.Text;
  hide;
  f_indigui.ShowModal;
  if not f_indigui.ConnectedServer then
    show
  else begin
    if f_indigui.DisconnectedServer then ShowMessage('Disconnected from server');
    close;
  end;
end;

procedure Tf_connect.MenuHelpClick(Sender: TObject);
var aboutmsg,cdate: string;
begin
cdate:={$I %DATE%};
cdate:=copy(cdate,1,4);
aboutmsg:='INDI Client '+starter_version+'-'+RevisionStr+crlf;
aboutmsg:=aboutmsg+'A simple INDI client GUI'+crlf;
aboutmsg:=aboutmsg+crlf;
aboutmsg:=aboutmsg+'Options:'+crlf;
aboutmsg:=aboutmsg+'-h --host hostname :'+tab+'INDI server host name'+crlf;
aboutmsg:=aboutmsg+'-p --port port :    '+tab+tab+'INDI server port number'+crlf;
aboutmsg:=aboutmsg+'-n --noconnect :    '+tab+tab+'Do not try to auto-connect to localhost'+crlf;
aboutmsg:=aboutmsg+crlf;
aboutmsg:=aboutmsg+'Copyright (C) '+cdate+' Patrick Chevalley'+crlf;
aboutmsg:=aboutmsg+'http://www.ap-i.net'+crlf;
aboutmsg:=aboutmsg+crlf;
aboutmsg:=aboutmsg+'This program is free software; you can redistribute it and/or'+crlf;
aboutmsg:=aboutmsg+'modify it under the terms of the GNU General Public License'+crlf;
aboutmsg:=aboutmsg+'as published by the Free Software Foundation; either version 3'+crlf;
aboutmsg:=aboutmsg+'of the License, or (at your option) any later version.'+crlf;
ShowMessage(aboutmsg);
end;


end.

