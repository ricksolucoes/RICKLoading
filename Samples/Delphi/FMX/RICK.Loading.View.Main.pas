unit RICK.Loading.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TForm1 = class(TForm)
    btnModified: TSpeedButton;
    btSimple: TSpeedButton;
    imgLogo: TImage;
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
  RICK.Loading.Interfaces,
  RICK.Loading;

procedure TForm1.btnModifiedClick(Sender: TObject);
var
  LRICKLoading: iRICKLoading;
begin
  LRICKLoading:= TRICKLoading.New.Form(Self);
  LRICKLoading.DoMessage('Loading Modified'); //Changes the initial loading message
  LRICKLoading.SourceSize(32); //Change the font size
  LRICKLoading.SourceName('Segoe UI'); //Change the font type
  LRICKLoading.SourceColor($FFF52121); //Change the font color
  LRICKLoading.AnimationColor($FFF52121); //Changes the color of the animation
  LRICKLoading.BackgroundColor($FF24CCC6); //Changes the color of the loading background
  LRICKLoading.OpacityBackground(0.9); //Changes the opacity of the background;
  LRICKLoading.OpacityAnimationText(0.6); //Change the opacity of text


  LRICKLoading.Execute(
  procedure
  begin

    TThread.Sleep(500);

    TThread.Synchronize(TThread.Current,
    procedure
    begin
      LRICKLoading.ChangeMessage('Changing message'); //Change the message to the user
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
  TRICKLoading.New
    .Form(Self) //Inform the form for Loading
      .Execute(
      procedure
      begin
        //Delayed Command
        Sleep(500);

        TThread.Synchronize(TThread.Current,
        procedure
        begin
          TRICKLoading.New.Form(Self)
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
