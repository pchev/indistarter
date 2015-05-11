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

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls;

type

  { Tf_setup }

  Tf_setup = class(TForm)
    autostart: TCheckBox;
    Button1: TButton;
    Button2: TButton;
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
    devlist: TFileNameEdit;
    Label1: TLabel;
    procedure remoteClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  f_setup: Tf_setup;

implementation

{$R *.lfm}

{ Tf_setup }

procedure Tf_setup.remoteClick(Sender: TObject);
begin
  PanelRemote.Visible:=remote.Checked;
  AutoSize:=false;
  AutoSize:=true;
end;

end.

