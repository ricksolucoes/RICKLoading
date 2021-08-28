unit RIBCarregar.View.Demostracao;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    btnModified: TSpeedButton;
    btSimple: TSpeedButton;
    procedure btnModifiedClick(Sender: TObject);
    procedure btSimpleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Rick.Loading.Interfaces,
  Rick.Loading.Register;

procedure TForm1.btnModifiedClick(Sender: TObject);
var
  LLoading: iLoading;
begin
  LLoading:= TLoading.New;
  LLoading.DoMessage('Loading Modified'); //Changes the initial loading message
  LLoading.SourceSize(32); //Change the font size
  LLoading.SourceName('Segoe UI'); //Change the font type
  LLoading.SourceColor($FFF52121); //Change the font color
  LLoading.AnimationColor($FFF52121); //Changes the color of the animation
  LLoading.BackgroundColor($FF24CCC6); //Changes the color of the loading background
  LLoading.OpacityBackground(0.9); //Changes the opacity of the background;
  LLoading.OpacityAnimationText(0.6); //Change the opacity of text


  LLoading.Execute(
  procedure
  begin

    TThread.Sleep(500);

    TThread.Synchronize(TThread.Current,
    procedure
    begin
      LLoading.ChangeMessage('Changing message'); //Change the message to the user
    end);

    TThread.Sleep(1500);


    TThread.Synchronize(TThread.Current,
    procedure
    begin
      ShowMessage('Command to refresh the screen here...');
    end);
  end);

end;

procedure TForm1.btSimpleClick(Sender: TObject);

begin
  TLoading.New
    .Execute(
    procedure
    begin
      //Delayed Command
      Sleep(500);

      TThread.Synchronize(TThread.Current,
      procedure
      begin
        TLoading.New
          .ChangeMessage('Changing message'); //Change the message to the user
      end);

      //Another command if there is one
      TThread.Sleep(1500);

      TThread.Synchronize(TThread.Current,
      procedure
      begin
        //Command to refresh the screen
        ShowMessage('Command to refresh the screen here...');
      end);
    end);
end;

end.
