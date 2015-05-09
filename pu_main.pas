unit pu_main;

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

uses pu_devlist, pu_setup, u_utils, UniqueInstance, XMLConf, process,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ColorBox,
  ComCtrls, StdCtrls, Grids, ExtCtrls, ActnList, Menus;

type

  { Tf_main }

  Tf_main = class(TForm)
    BtnAdd: TButton;
    BtnStartStop: TButton;
    Image1: TImage;
    ImageList1: TImageList;
    LabelStatus: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuItem1: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    MenuEditName: TMenuItem;
    MenuSetup2: TMenuItem;
    MenuSetup: TMenuItem;
    MenuRestartDevice: TMenuItem;
    MenuStopDevice: TMenuItem;
    MenuDeleteDevice: TMenuItem;
    MenuRestartServer: TMenuItem;
    MenuStopServer: TMenuItem;
    MenuQuit: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    StringGrid1: TStringGrid;
    StatusTimer: TTimer;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnStartStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuDeleteDeviceClick(Sender: TObject);
    procedure MenuEditNameClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuRestartDeviceClick(Sender: TObject);
    procedure MenuRestartServerClick(Sender: TObject);
    procedure MenuSetupClick(Sender: TObject);
    procedure MenuStopDeviceClick(Sender: TObject);
    procedure MenuStopServerClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StatusTimerTimer(Sender: TObject);
  private
    { private declarations }
    config: TXMLConfig;
    ConfigDir,configfile,devlist,serveroptions: string;
    autostart,stayontop: boolean;
    ServerFifo: string;
    ServerProcess: TProcess;
    CurrentCol, CurrentRow: integer;
    UniqueInstance1: TCdCUniqueInstance;
    procedure OtherInstance(Sender : TObject; ParamCount: Integer; Parameters: array of String);
    procedure InstanceRunning(Sender : TObject);
    procedure ClearGrid;
    procedure SaveConfig;
    function  WriteCmd(cmd:string): boolean;
    procedure CheckDuplicateDevice(dev: Tdevicenode);
    procedure EditDeviceName(r: integer);
    procedure StartDevice(r:integer);
    procedure StopDevice(r:integer);
    procedure StartServer;
    procedure StopServer;
    procedure Status;
  public
    { public declarations }
  end;

var
  f_main: Tf_main;

implementation

{$R *.lfm}

{ Tf_main }

procedure Tf_main.FormCreate(Sender: TObject);
var i:integer;
begin
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.TimeSeparator:=':';
  UniqueInstance1:=TCdCUniqueInstance.Create(self);
  UniqueInstance1.Identifier:='IndiStarter';
  UniqueInstance1.OnOtherInstance:=@OtherInstance;
  UniqueInstance1.OnInstanceRunning:=@InstanceRunning;
  UniqueInstance1.Enabled:=true;
  UniqueInstance1.Loaded;

  ServerFifo:=slash(GetTempDir(true))+'IndiStarter.fifo';
  ClearGrid;
  ConfigExtension:= '.conf';
  config:=TXMLConfig.Create(self);
  ConfigDir:=GetAppConfigDirUTF8(false,true);
  if Application.HasOption('c', 'config') then begin
    configfile:=Application.GetOptionValue('c', 'config')+'.conf';
  end
  else configfile:='default.conf';
  config.Filename:=slash(ConfigDir)+configfile;
  devlist:=slash(ConfigDir)+ChangeFileExt(configfile,'.devices');
  devlist:=config.GetValue('/Devices/List',devlist);
  autostart:=config.GetValue('/Server/Autostart',false);
  serveroptions:=config.GetValue('/Server/Options','');
  stayontop:=config.GetValue('/Window/StayOnTop',true);
  if FileExistsUTF8(devlist) then StringGrid1.LoadFromCSVFile(devlist);
  if autostart then StartServer;
  if stayontop then FormStyle:=fsStayOnTop else FormStyle:=fsNormal;
end;

procedure Tf_main.OtherInstance(Sender : TObject; ParamCount: Integer; Parameters: array of String);
begin
  BringToFront;
end;

procedure Tf_main.InstanceRunning(Sender : TObject);
begin
  writeln('Other instance of indistarter is running?');
  UniqueInstance1.RetryOrHalt;
end;

procedure Tf_main.FormDestroy(Sender: TObject);
begin
  if (ServerProcess<>nil) and ServerProcess.Running then
        ServerProcess.Terminate(0);
end;

procedure Tf_main.MenuAboutClick(Sender: TObject);
var aboutmsg: string;
begin
aboutmsg:='INDI Starter '+crlf;
aboutmsg:=aboutmsg+starter_version+crlf+crlf;
aboutmsg:=aboutmsg+'A simple program to run a INDI server'+crlf;
aboutmsg:=aboutmsg+'http://www.indilib.org'+crlf+crlf;
aboutmsg:=aboutmsg+'Copyright (C) 2015 Patrick Chevalley'+crlf;
aboutmsg:=aboutmsg+'http://www.ap-i.net'+crlf+crlf;
aboutmsg:=aboutmsg+'This program is free software; you can redistribute it and/or'+crlf;
aboutmsg:=aboutmsg+'modify it under the terms of the GNU General Public License'+crlf;
aboutmsg:=aboutmsg+'as published by the Free Software Foundation; either version 3'+crlf;
aboutmsg:=aboutmsg+'of the License, or (at your option) any later version.'+crlf;
ShowMessage(aboutmsg);
end;

procedure Tf_main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ServerProcess=nil then begin
     SaveConfig;
     CloseAction:=caFree;
  end else begin
     CloseAction:=caMinimize;
  end;
end;

procedure Tf_main.SaveConfig;
begin
  StringGrid1.SaveToCSVFile(devlist);
  config.SetValue('/Devices/List',devlist);
  config.SetValue('/Server/Autostart',autostart);
  config.SetValue('/Server/Options',serveroptions);
  config.SetValue('/Window/StayOnTop',stayontop);
  config.Flush;
end;

procedure Tf_main.ClearGrid;
begin
  StringGrid1.RowCount:=1;
  StringGrid1.ColWidths[0]:=23;
  StringGrid1.ColWidths[1]:=110;
  StringGrid1.ColWidths[2]:=156;
  StringGrid1.ColWidths[3]:=150;
  StringGrid1.Cells[0,0]:='';
  StringGrid1.Cells[1,0]:='Group';
  StringGrid1.Cells[2,0]:='Driver name';
  StringGrid1.Cells[3,0]:='Driver';
end;

procedure Tf_main.MenuSetupClick(Sender: TObject);
var savedevlist: string;
  savestayontop:boolean;
begin
 if ServerProcess=nil then begin
  SaveConfig;
  savedevlist:=devlist;
  savestayontop:=stayontop;
  f_setup.devlist.DefaultExt:='.devices';
  f_setup.devlist.InitialDir:=ConfigDir;
  f_setup.devlist.FileName:=devlist;
  f_setup.serveroptions.Text:=serveroptions;
  f_setup.autostart.Checked:=autostart;
  f_setup.stayontop.Checked:=stayontop;
  FormPos(f_setup,Mouse.CursorPos.X,Mouse.CursorPos.Y);
  f_setup.ShowModal;
  if f_setup.ModalResult=mrOK then begin
    devlist   := f_setup.devlist.FileName;
    if (devlist<>savedevlist) then begin
      if FileExistsUTF8(devlist) then
         StringGrid1.LoadFromCSVFile(devlist)
      else
         ClearGrid;
    end;
    autostart := f_setup.autostart.Checked;
    serveroptions := f_setup.serveroptions.Text;
    stayontop := f_setup.stayontop.Checked;
    if (stayontop<>savestayontop) then begin
      if stayontop then FormStyle:=fsStayOnTop else FormStyle:=fsNormal;
    end;
    SaveConfig;
  end;
 end
 else
   ShowMessage('Stop the server before to change the setup.');
end;


procedure Tf_main.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_main.BtnStartStopClick(Sender: TObject);
begin
 if BtnStartStop.Caption='Start' then begin
    StartServer;
 end else begin
    if MessageDlg('Stop INDI server','Do you want to stop the INDI server now?',mtConfirmation,mbYesNo,0)=mrYes then
       StopServer;
 end;

end;

procedure Tf_main.MenuRestartServerClick(Sender: TObject);
begin
  try
  Screen.Cursor:=crHourGlass;
  StopServer;
  Wait(2);
  StartServer;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tf_main.MenuStopServerClick(Sender: TObject);
begin
  StopServer;
end;

procedure Tf_main.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with Sender as TStringGrid do begin
    if (aCol=0)and(aRow>0) then begin
      Canvas.Brush.style := bssolid;
      if (cells[aCol,aRow]='1')then begin
        Canvas.Brush.Color := clWindow;
        Canvas.FillRect(aRect);
        ImageList1.Draw(Canvas,aRect.left+2,aRect.top+2,1);
      end else begin
        Canvas.Brush.Color := clWindow;
        Canvas.FillRect(aRect);
        ImageList1.Draw(Canvas,aRect.left+2,aRect.top+2,0);
      end;
    end;
  end;
end;

procedure Tf_main.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var grsel:TGridRect;
begin
  StringGrid1.MouseToCell(X, Y, CurrentCol, CurrentRow);
  grsel.Top:=CurrentRow;
  grsel.Bottom:=CurrentRow;
  grsel.Left:=CurrentCol;
  grsel.Right:=CurrentCol;
  StringGrid1.Selection:=grsel;
end;

procedure Tf_main.PopupMenu1Popup(Sender: TObject);
begin
 if ServerProcess=nil then begin
    MenuRestartDevice.Caption:='Start server';
 end
 else begin
 if (CurrentRow>0)and(CurrentRow<StringGrid1.RowCount)and(StringGrid1.Cells[0,CurrentRow]='1') then
    MenuRestartDevice.Caption:='Restart device'
 else
    MenuRestartDevice.Caption:='Start device';
 end;
end;

procedure Tf_main.StatusTimerTimer(Sender: TObject);
begin
  Status;
end;

procedure Tf_main.MenuRestartDeviceClick(Sender: TObject);
begin
  try
  Screen.Cursor:=crHourGlass;
  if ServerProcess=nil then
     StartServer
  else
     StopDevice(CurrentRow);
  wait(2);
  StartDevice(CurrentRow);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tf_main.MenuStopDeviceClick(Sender: TObject);
begin
  try
  Screen.Cursor:=crHourGlass;
  StopDevice(CurrentRow);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tf_main.BtnAddClick(Sender: TObject);
var node: TTreeNode;
    dev: Tdevicenode;
    r:integer;
begin
  FormPos(f_devlist,Mouse.CursorPos.X,Mouse.CursorPos.Y);
  f_devlist.ShowModal;
  if (f_devlist.ModalResult=mrOK) then begin
      node:=f_devlist.TreeView1.Selected;
      if node<>nil then begin
         dev:=Tdevicenode(node.Data);
         if dev<>nil then begin
            CheckDuplicateDevice(dev);
            StringGrid1.RowCount:=StringGrid1.RowCount+1;
            r:=StringGrid1.RowCount-1;
            StringGrid1.Cells[0,r]:='';
            StringGrid1.Cells[1,r]:=dev.GroupName;
            StringGrid1.Cells[2,r]:=dev.DevLbl;
            StringGrid1.Cells[3,r]:=dev.Drv;
            if ServerProcess<>nil then StartDevice(r);
         end;
      end;
      StringGrid1.SaveToCSVFile(devlist);
  end;
end;

procedure Tf_main.MenuDeleteDeviceClick(Sender: TObject);
var i:integer;
begin
  if (StringGrid1.RowCount>1)and(CurrentRow>=1)and(CurrentRow<StringGrid1.RowCount) then begin
     try
     Screen.Cursor:=crHourGlass;
     StopDevice(CurrentRow);
     for i:=CurrentRow to StringGrid1.RowCount-2 do begin
       StringGrid1.Cells[0,i]:=StringGrid1.Cells[0,i+1];
       StringGrid1.Cells[1,i]:=StringGrid1.Cells[1,i+1];
       StringGrid1.Cells[2,i]:=StringGrid1.Cells[2,i+1];
       StringGrid1.Cells[3,i]:=StringGrid1.Cells[3,i+1];
     end;
     StringGrid1.RowCount:=StringGrid1.RowCount-1;
     StringGrid1.SaveToCSVFile(devlist);
     finally
       Screen.Cursor:=crDefault;
     end;
  end;
end;

procedure Tf_main.MenuEditNameClick(Sender: TObject);
begin
 if StringGrid1.Cells[0,CurrentRow]<>'1' then
    EditDeviceName(CurrentRow)
 else
    ShowMessage('The device must be stopped.');
end;

procedure Tf_main.CheckDuplicateDevice(dev: Tdevicenode);
var drvname,devname: string;
    i: integer;
begin
  drvname:=trim(dev.Drv);
  devname:=trim(dev.DevLbl);
  for i:=1 to StringGrid1.RowCount-1 do begin
    if (trim(StringGrid1.Cells[3,i])=drvname)and(trim(StringGrid1.Cells[2,i])=devname) then begin
       while trim(StringGrid1.Cells[2,i])=devname do
            devname:=FormEntry(self,'Duplicate, enter new name',devname);
       dev.DevLbl:=devname;
       break;
    end;
  end;
end;

procedure Tf_main.EditDeviceName(r: integer);
var drvname,devname: string;
    i: integer;
begin
 if (r>0)and(r<StringGrid1.RowCount) then begin
  drvname:=trim(StringGrid1.Cells[3,r]);
  devname:=trim(StringGrid1.Cells[2,r]);
  devname:=FormEntry(self,'Enter new name',devname);
  for i:=1 to StringGrid1.RowCount-1 do begin
    if (i<>r)and(trim(StringGrid1.Cells[3,i])=drvname)and(trim(StringGrid1.Cells[2,i])=devname) then begin
       while trim(StringGrid1.Cells[2,i])=devname do
            devname:=FormEntry(self,'Duplicate, enter new name',devname);
       break;
    end;
  end;
  StringGrid1.Cells[2,r]:=devname;
 end;
end;

procedure Tf_main.StartDevice(r:integer);
var drv,devname,buf: string;
begin
  if (r>0)and(r<StringGrid1.RowCount) then begin
     drv:=StringGrid1.Cells[3,r];
     devname:=StringGrid1.Cells[2,r];
     buf:='start '+drv+' -n "'+devname+'"';
     WriteCmd(buf);
     StringGrid1.Cells[0,r]:='1';
  end;
end;

procedure Tf_main.StopDevice(r:integer);
var drv,devname,buf: string;
begin
  if (r>0)and(r<StringGrid1.RowCount) then begin
     drv:=StringGrid1.Cells[3,r];
     devname:=StringGrid1.Cells[2,r];
     buf:='stop '+drv+' -n "'+devname+'"';
     WriteCmd(buf);
     StringGrid1.Cells[0,r]:='';
  end;
end;

function Tf_main.WriteCmd(cmd:string): boolean;
var f: textfile;
begin
  AssignFile(f,ServerFifo);
  Rewrite(f);
  Writeln(f,cmd);
  CloseFile(f);
end;

procedure Tf_main.StartServer;
var str:TStringList;
    buf:string;
    i:integer;
begin
  if ServerProcess=nil then begin
     str:=TStringList.Create;
     DeleteFile(ServerFifo);
     if (ExecProcess('mkfifo '+ServerFifo,str)=0) then begin
        ServerProcess:=ExecProcessNoWait('indiserver '+serveroptions+' -f '+ServerFifo);
        Wait(1);
        if StringGrid1.RowCount>1 then begin
           for i:=1 to StringGrid1.RowCount-1 do begin
              StartDevice(i);
           end;
        end;
     end else begin
        buf:='Cannot create FIFO!'+crlf;
        for i:=0 to str.Count-1 do buf:=buf+str[i]+crlf;
        ShowMessage(buf);
     end;
     str.free;
  end;
  Status;
end;

procedure Tf_main.StopServer;
var i:integer;
begin
  if ServerProcess<>nil then begin
     if ServerProcess.Running then
        ServerProcess.Terminate(0);
     FreeAndNil(ServerProcess);
     for i:=1 to StringGrid1.RowCount-1 do begin
        StringGrid1.Cells[0,i]:='';
     end;
  end;
  DeleteFile(ServerFifo);
  Status;
end;

procedure Tf_main.Status;
var buf: string;
    str: TStringList;
    i: integer;
begin
  if ServerProcess<>nil then begin
    if ServerProcess.Running then begin
       ImageList1.GetBitmap(1,image1.Picture.Bitmap);
       BtnStartStop.Caption:='Stop';
       LabelStatus.Caption:='Server running';
       MenuRestartServer.Caption:='&Restart server';
       MenuQuit.Caption:='&Minimize';
    end else begin
       ServerProcess:=nil;
       ImageList1.GetBitmap(0,image1.Picture.Bitmap);
       BtnStartStop.Caption:='Start';
       LabelStatus.Caption:='Server stopped';
       MenuRestartServer.Caption:='St&art server';
       MenuQuit.Caption:='&Quit';
    end;
    str:=TStringList.Create;
    for i:=1 to StringGrid1.RowCount-1 do begin
       if (i<StringGrid1.RowCount)and (ExecProcess('pgrep -f '+StringGrid1.Cells[3,i],str)=0)
          then StringGrid1.Cells[0,i]:='1'
          else StringGrid1.Cells[0,i]:='';
    end;
    str.Free;
  end
  else begin
    ImageList1.GetBitmap(0,image1.Picture.Bitmap);
    BtnStartStop.Caption:='Start';
    LabelStatus.Caption:='Server stopped';
    MenuRestartServer.Caption:='St&art server';
    MenuQuit.Caption:='&Quit';
    for i:=1 to StringGrid1.RowCount-1 do begin
       StringGrid1.Cells[0,i]:='';
    end;
  end;
end;

end.

