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

uses pu_devlist, pu_setup, u_utils, pu_indigui, UniqueInstance, XMLConf, process,
  indibaseclient, indibasedevice, UScaleDPI,
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Grids, ExtCtrls, ActnList, Menus;

type

  { Tf_main }

  Tf_main = class(TForm)
    BtnAdd: TButton;
    BtnStartStop: TButton;
    ClientBtn: TButton;
    ConfigLabel: TLabel;
    Label1: TLabel;
    led: TImage;
    ImageList1: TImageList;
    LabelStatus: TLabel;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuItem1: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    MenuEditName: TMenuItem;
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
    procedure ClientBtnClick(Sender: TObject);
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
    f_indigui: Tf_indigui;
    indiclient: TIndiBaseClient;
    ActiveDevLst: TStringList;
    TunnelProcess: TProcess;
    rc,config: TXMLConfig;
    ConfigDir,configfile,devlist,serveroptions: string;
    RemoteHost,RemoteUser,LocalPort,RemotePort,sshopt: string;
    autostart,stayontop,remote,ServerStarted: boolean;
    GUIready: boolean;
    ServerFifo: string;
    CurrentCol, CurrentRow: integer;
    UniqueInstance1: TCdCUniqueInstance;
    procedure LoadConfig(cname:string);
    procedure OtherInstance(Sender : TObject; ParamCount: Integer; Parameters: array of String);
    procedure InstanceRunning(Sender : TObject);
    procedure ClearGrid;
    procedure SaveConfig;
    function  WriteCmd(cmd:string): boolean;
    procedure ShowErr(msg:string; str:TStringList);
    procedure CheckDuplicateDevice(dev: Tdevicenode);
    procedure EditDeviceName(r: integer);
    procedure StartDevice(r:integer);
    procedure StopDevice(r:integer);
    procedure StartServer;
    procedure StopServer;
    procedure StartTunnel;
    procedure StopTunnel;
    function  ServerPid: string;
    procedure Status;
    function GetServerPort: string;
    procedure GUIdestroy(Sender: TObject);
    procedure SetupConfigChange(Sender: TObject);
    procedure GetIndiDevices;
    procedure IndiNewDevice(dp: Basedevice);
    procedure IndiDeleteDevice(dp: Basedevice);
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
  ServerStarted:=true; // to check if the server is already started
  UniqueInstance1:=TCdCUniqueInstance.Create(self);
  UniqueInstance1.Identifier:='IndiStarter';
  UniqueInstance1.OnOtherInstance:=@OtherInstance;
  UniqueInstance1.OnInstanceRunning:=@InstanceRunning;
  UniqueInstance1.Enabled:=true;
  UniqueInstance1.Loaded;
  ActiveDevLst:=TStringList.Create;
  sshopt:=' -oBatchMode=yes -oConnectTimeout=10 ';
  ServerFifo:=slash(GetTempDir(true))+'IndiStarter.fifo';
  ClientBtn.Enabled:=false;
  autostart:=false;
  serveroptions:='';
  remote:=false;
  RemoteHost:='';
  RemoteUser:='';
  LocalPort:='7624';
  RemotePort:='7624';
  stayontop:=false;
  ClearGrid;
  ConfigExtension:= '.conf';
  rc:=TXMLConfig.Create(self);
  config:=TXMLConfig.Create(self);
  ConfigDir:=GetAppConfigDirUTF8(false,true);
  configfile:='default.conf';
  devlist:=slash(ConfigDir)+'default.devices';
  rc.Filename:=slash(ConfigDir)+'indistarter.rc';
  configfile:=rc.GetValue('/Current/Config',configfile);
  if Application.HasOption('c', 'config') then begin
    configfile:=Application.GetOptionValue('c', 'config')+'.conf';
  end;
  LoadConfig(configfile);
  UScaleDPI.UseScaling:=true;
  UScaleDPI.SetScale(Canvas);
  ScaleDPI(Self);
  if autostart then StartServer;
end;

procedure Tf_main.LoadConfig(cname:string);
var i: integer;
begin
  ConfigLabel.Caption:=ExtractFileNameOnly(cname);
  configfile:=cname;
  config.Filename:=slash(ConfigDir)+configfile;
  devlist:=config.GetValue('/Devices/List',devlist);
  autostart:=config.GetValue('/Server/Autostart',autostart);
  serveroptions:=config.GetValue('/Server/Options',serveroptions);
  remote:=config.GetValue('/Server/Remote',remote);
  RemoteHost:=config.GetValue('/RemoteServer/Host',RemoteHost);
  RemoteUser:=config.GetValue('/RemoteServer/User',RemoteUser);
  LocalPort:=config.GetValue('/RemoteServer/LocalPort',LocalPort);
  RemotePort:=config.GetValue('/RemoteServer/RemotePort',RemotePort);
  stayontop:=config.GetValue('/Window/StayOnTop',stayontop);
  if FileExistsUTF8(devlist) then StringGrid1.LoadFromCSVFile(devlist);
  ActiveDevLst.Clear;
  for i:=1 to StringGrid1.RowCount-1 do begin
     StringGrid1.Cells[0,i]:='';
  end;
  if stayontop then FormStyle:=fsStayOnTop else FormStyle:=fsNormal;
  if remote then StatusTimer.Interval:=15000 else StatusTimer.Interval:=5000;
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
  if ServerPid<>'' then StopServer;
  ActiveDevLst.Free;
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
  if ServerPid='' then begin
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
  config.SetValue('/Server/Remote',remote);
  config.SetValue('/RemoteServer/Host',RemoteHost);
  config.SetValue('/RemoteServer/User',RemoteUser);
  config.SetValue('/RemoteServer/LocalPort',LocalPort);
  config.SetValue('/RemoteServer/RemotePort',RemotePort);
  config.SetValue('/Window/StayOnTop',stayontop);
  config.Flush;
  rc.SetValue('/Current/Config',configfile);
  rc.Flush;
end;

procedure Tf_main.ClearGrid;
begin
  StringGrid1.RowCount:=1;
  StringGrid1.ColWidths[0]:=23;
  StringGrid1.ColWidths[1]:=110;
  StringGrid1.ColWidths[2]:=156;
  StringGrid1.ColWidths[3]:=250;
  StringGrid1.Cells[0,0]:='';
  StringGrid1.Cells[1,0]:='Group';
  StringGrid1.Cells[2,0]:='Driver name';
  StringGrid1.Cells[3,0]:='Driver';
end;

procedure Tf_main.MenuSetupClick(Sender: TObject);
var savedevlist: string;
  savestayontop:boolean;
begin
 if ServerPid='' then begin
  SaveConfig;
  savedevlist:=devlist;
  savestayontop:=stayontop;
  f_setup.onConfigChange:=@SetupConfigChange;
  f_setup.ConfigDir:=ConfigDir;
  f_setup.config:=ExtractFileNameOnly(configfile);
  f_setup.devlist:=ExtractFileNameOnly(devlist);
  f_setup.serveroptions.Text:=serveroptions;
  f_setup.autostart.Checked:=autostart;
  f_setup.stayontop.Checked:=stayontop;
  f_setup.remote.Checked:=remote;
  f_setup.remotehost.Text:=RemoteHost;
  f_setup.remoteuser.Text:=RemoteUser;
  f_setup.localport.Text:=LocalPort;
  f_setup.remoteport.Text:=RemotePort;
  f_setup.PanelRemote.Visible:=remote;
  FormPos(f_setup,Mouse.CursorPos.X,Mouse.CursorPos.Y);
  f_setup.ShowModal;
  if f_setup.ModalResult=mrOK then begin
    devlist:=slash(ConfigDir)+f_setup.devlist+'.devices';
    if (devlist<>savedevlist) then begin
      if FileExistsUTF8(devlist) then
         StringGrid1.LoadFromCSVFile(devlist)
      else
         ClearGrid;
    end;
    autostart := f_setup.autostart.Checked;
    serveroptions := f_setup.serveroptions.Text;
    remote     := f_setup.remote.Checked;
    RemoteHost := f_setup.remotehost.Text;
    RemoteUser := f_setup.remoteuser.Text;
    LocalPort  := f_setup.localport.Text;
    RemotePort := f_setup.remoteport.Text;
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

procedure Tf_main.SetupConfigChange(Sender: TObject);
begin

 LoadConfig(f_setup.config+'.conf');
 f_setup.devlist:=ExtractFileNameOnly(devlist);
 f_setup.serveroptions.Text:=serveroptions;
 f_setup.autostart.Checked:=autostart;
 f_setup.stayontop.Checked:=stayontop;
 f_setup.remote.Checked:=remote;
 f_setup.remotehost.Text:=RemoteHost;
 f_setup.remoteuser.Text:=RemoteUser;
 f_setup.localport.Text:=LocalPort;
 f_setup.remoteport.Text:=RemotePort;
 f_setup.PanelRemote.Visible:=remote;
 f_setup.UpdDevices;
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
 if ServerPid='' then begin
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
  StatusTimer.Enabled:=false;
  Status;
  if remote then StatusTimer.Interval:=15000 else StatusTimer.Interval:=2000;
  StatusTimer.Enabled:=true;
end;

procedure Tf_main.MenuRestartDeviceClick(Sender: TObject);
begin
  try
  Screen.Cursor:=crHourGlass;
  if ServerPid='' then
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
    cdrv,chost,buf: string;
begin
  f_devlist.CustomCheckBox.Checked:=false;
  FormPos(f_devlist,Mouse.CursorPos.X,Mouse.CursorPos.Y);
  f_devlist.ShowModal;
  if (f_devlist.ModalResult=mrOK) then begin
    if f_devlist.CustomCheckBox.Checked then begin
      cdrv:=trim(f_devlist.CustomDriver.Text);
      if cdrv<>'' then begin
        chost:=trim(f_devlist.CustomHost.Text);
        if chost='' then
           buf:=cdrv
        else
           buf:='"'+cdrv+'"@'+chost;
        StringGrid1.RowCount:=StringGrid1.RowCount+1;
        r:=StringGrid1.RowCount-1;
        StringGrid1.Cells[0,r]:='';
        StringGrid1.Cells[1,r]:='Custom';
        StringGrid1.Cells[2,r]:=cdrv;
        StringGrid1.Cells[3,r]:=buf;
        if ServerPid<>'' then StartDevice(r);
      end;
    end else begin
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
            if ServerPid<>'' then StartDevice(r);
         end;
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

procedure Tf_main.ShowErr(msg:string; str:TStringList);
var buf: string;
    i:integer;
begin
  buf:=msg+crlf;
  for i:=0 to str.Count-1 do buf:=buf+str[i]+crlf;
  ShowMessage(buf);
end;

procedure Tf_main.StartDevice(r:integer);
var group,drv,devname,buf: string;
begin
  if (r>0)and(r<StringGrid1.RowCount) then begin
     group:=StringGrid1.Cells[1,r];
     drv:=StringGrid1.Cells[3,r];
     devname:=StringGrid1.Cells[2,r];
     if group='Custom' then begin
       if remote then
         buf:='start '+drv
       else
         buf:='start '+drv;
     end else begin
       if remote then
         buf:='start '+drv+' -n \"'+devname+'\"'
       else
         buf:='start '+drv+' -n "'+devname+'"';
     end;
     WriteCmd(buf);
  end;
end;

procedure Tf_main.StopDevice(r:integer);
var group,drv,devname,buf: string;
begin
  if (r>0)and(r<StringGrid1.RowCount) then begin
     group:=StringGrid1.Cells[1,r];
     drv:=StringGrid1.Cells[3,r];
     devname:=StringGrid1.Cells[2,r];
    if group='Custom' then begin
       if remote then
         buf:='stop '+drv
       else
         buf:='stop '+drv;
    end else begin
       if remote then
         buf:='stop '+drv+' -n \"'+devname+'\"'
       else
         buf:='stop '+drv+' -n "'+devname+'"';
     end;
     WriteCmd(buf);
  end;
end;

function Tf_main.WriteCmd(cmd:string): boolean;
var f: textfile;
    str:TStringList;
begin
 if ServerPid<>'' then begin
  if remote then begin
    str:=TStringList.Create;
    ExecProcess('ssh '+sshopt+RemoteUser+'@'+RemoteHost+' echo '+cmd+'>'+ServerFifo,str);
    result:=true;
    str.Free;
  end
  else begin
    AssignFile(f,ServerFifo);
    Rewrite(f);
    Writeln(f,cmd);
    CloseFile(f);
    result:=true;
  end;
 end
 else
   result:=false;
end;

procedure Tf_main.StartServer;
var str:TStringList;
    buf:string;
    i,r:integer;
begin
try
if StringGrid1.RowCount<2 then begin
  LabelStatus.Caption:='No driver configured!';
  exit;
end;
StatusTimer.Enabled:=false;
  if ServerPid='' then begin
     str:=TStringList.Create;
     if remote then begin
        ExecProcess('ssh '+sshopt+RemoteUser+'@'+RemoteHost+' rm '+ServerFifo,str);
        if ExecProcess('ssh '+sshopt+RemoteUser+'@'+RemoteHost+' mkfifo '+ServerFifo,str)<>0 then begin ShowErr(RemoteUser+'@'+RemoteHost+' mkfifo '+ServerFifo,str);exit;end;
        if ExecProcess('ssh '+sshopt+RemoteUser+'@'+RemoteHost+' "sh -c ''nohup indiserver '+serveroptions+' -f '+ServerFifo+' >/dev/null 2>&1 &''"',str)<>0 then begin ShowErr(RemoteUser+'@'+RemoteHost+' indiserver',str);exit;end;
        Wait(5);
        ServerStarted:=true;
        if StringGrid1.RowCount>1 then begin
           for i:=1 to StringGrid1.RowCount-1 do begin
              StartDevice(i);
           end;
        end;
        StartTunnel;
     end
     else begin
       DeleteFile(ServerFifo);
       if (ExecProcess('mkfifo '+ServerFifo,str)=0) then begin
          r:=ExecBG('indiserver '+serveroptions+' -f '+ServerFifo);
          Wait(1);
          ServerStarted:=true;
          if StringGrid1.RowCount>1 then begin
             for i:=1 to StringGrid1.RowCount-1 do begin
                StartDevice(i);
             end;
          end;
       end else begin
          ShowErr('Cannot create FIFO!',str);
          exit;
       end;
     end;
     str.free;
     ActiveDevLst.Clear;
     indiclient:=TIndiBaseClient.Create;
     indiclient.onNewDevice:=@IndiNewDevice;
     indiclient.onDeleteDevice:=@IndiDeleteDevice;
     indiclient.SetServer('localhost',GetServerPort);
     indiclient.ConnectServer;
  end;
  Status;
finally
  StatusTimer.Enabled:=true;
end;
end;

procedure Tf_main.StopServer;
var i:integer;
    str:TStringList;
begin
     if indiclient<>nil then indiclient.DisconnectServer;
     str:=TStringList.Create;
     if remote then begin
        StopTunnel;
        ExecProcess('ssh '+sshopt+RemoteUser+'@'+RemoteHost+' killall indiserver ',str);
        ExecProcess('ssh '+sshopt+RemoteUser+'@'+RemoteHost+' rm '+ServerFifo,str);
     end
     else begin
        ExecProcess('killall indiserver',str);
        DeleteFile(ServerFifo);
        for i:=1 to StringGrid1.RowCount-1 do begin
           StringGrid1.Cells[0,i]:='';
        end;
     end;
     str.free;
     ServerStarted:=false;
     ClientBtn.Enabled:=false;
     ImageList1.GetBitmap(0,led.Picture.Bitmap);
     BtnStartStop.Caption:='Start';
     LabelStatus.Caption:='Server stopped';
     MenuRestartServer.Caption:='St&art server';
     MenuQuit.Caption:='&Quit';
     ActiveDevLst.Clear;
     for i:=1 to StringGrid1.RowCount-1 do begin
        StringGrid1.Cells[0,i]:='';
     end;
end;

procedure Tf_main.StartTunnel;
begin
  if remote then begin
     TunnelProcess:=ExecProcessNoWait('ssh '+sshopt+' -N -L'+LocalPort+':'+RemoteHost+':'+RemotePort+' '+RemoteUser+'@'+RemoteHost);
     Wait(5);
  end;
end;

procedure Tf_main.StopTunnel;
begin
  if remote then begin
     if (TunnelProcess<>nil) and TunnelProcess.Running then begin
       TunnelProcess.Terminate(0);
       FreeAndNil(TunnelProcess);
     end;
  end;
end;

function  Tf_main.ServerPid: string;
var str: TStringList;
    i,c: integer;
begin
  if ServerStarted then begin
    try
    str:=TStringList.Create;
    try
    c:=0;
    repeat
      if remote then begin
        i:=ExecProcess('ssh '+sshopt+RemoteUser+'@'+RemoteHost+' pidof indiserver',str);
      end
      else begin
         i:=ExecProcess('pidof indiserver',str);
      end;
      inc(c);
      if i<>0 then sleep(100);
    until (i=0)or(c>3);
    if (i=0)and(str.Count>0) then
       result:=str[0]
    else
       result:='';
    finally
      str.Free;
    end;
    except
      result:='';
    end;
  end else
     result:='';
end;

procedure Tf_main.Status;
var str: TStringList;
    i: integer;
begin
  if ServerPid<>'' then begin
    ImageList1.GetBitmap(1,led.Picture.Bitmap);
    BtnStartStop.Caption:='Stop';
    ClientBtn.Enabled:=true;
    if remote then
      LabelStatus.Caption:='Server running on '+RemoteHost
    else
      LabelStatus.Caption:='Server running';
    MenuRestartServer.Caption:='&Restart server';
    MenuQuit.Caption:='&Minimize';
    if StringGrid1.RowCount>1 then begin
      GetIndiDevices;
    end;
  end
  else begin
    if BtnStartStop.Caption='Start' then
      LabelStatus.Caption:='Server stopped'
    else
      LabelStatus.Caption:='Server not running?';
    ImageList1.GetBitmap(0,led.Picture.Bitmap);
    ActiveDevLst.Clear;
    for i:=1 to StringGrid1.RowCount-1 do begin
       StringGrid1.Cells[0,i]:='';
    end;
  end;
end;

function Tf_main.GetServerPort: string;
var i: integer;
    buf: string;
begin
 if remote then begin
   result:=LocalPort;
 end
 else begin
   i:=pos('-p',serveroptions);
   if i<=0 then result:='7624'
   else begin
     buf:=serveroptions;
     delete(buf,1,i+1);
     i:=pos('-',buf);
     if i>0 then begin
       buf:=Copy(buf,1,i-1);
     end;
     result:=trim(buf);
     if result='' then result:='7624';
   end;
 end;
end;

procedure Tf_main.ClientBtnClick(Sender: TObject);
begin
 if not GUIready then begin
    f_indigui:=Tf_indigui.Create(self);
    f_indigui.onDestroy:=@GUIdestroy;
    f_indigui.IndiServer:='localhost';
    f_indigui.IndiPort:=GetServerPort;
    f_indigui.IndiDevice:='';
    GUIready:=true;
 end;
 FormPos(f_indigui,mouse.cursorpos.x,mouse.cursorpos.y);
 f_indigui.Show;
end;

procedure Tf_main.GUIdestroy(Sender: TObject);
begin
  GUIready:=false;
end;

procedure Tf_main.GetIndiDevices;
var i: integer;
begin
 if StringGrid1.RowCount>1 then
   for i:=1 to StringGrid1.RowCount-1 do begin
    if ( ActiveDevLst.IndexOf(StringGrid1.Cells[2,i])>=0)
       then StringGrid1.Cells[0,i]:='1'
       else StringGrid1.Cells[0,i]:='';
   end;
end;

procedure Tf_main.IndiNewDevice(dp: Basedevice);
begin
  ActiveDevLst.Add(dp.getDeviceName);
end;

procedure Tf_main.IndiDeleteDevice(dp: Basedevice);
var i: integer;
begin
  i:=ActiveDevLst.IndexOf(dp.getDeviceName);
  if i>=0 then ActiveDevLst.Delete(i);
end;

end.

