# RICKLoading

<p align="center"> <img src="https://user-images.githubusercontent.com/79030707/131561139-368fe33f-aac8-49bc-9aaf-3239b15e96b7.png" height=45% width=45%></p> 
<p align="center">
  <b>RICKLoading</b> is a Delphi library developed to present a loading screen. Using delphi's Fluent Interface.<br> 
  Inspired by Master Heber in his class (https://www.youtube.com/watch?v=AvNXYopib3Q&t=511s), <br>
  on channel 99Coders (https://www.youtube.com/channel/UCExEoXjNTmKLFLK2qpBSI5g)
</p>


<h2>⚙️ Install</h2>
<p>*Pre-requisites Delphi FMX</p>
<li><strong>Installation with BOSS</strong>: <br>
<pre>$ boss install https://github.com/ricksolucoes/rickloading</pre>

<li><strong>Manual Installation</strong>: <br>
Add the following folders to your project, in <em>Project &gt; Options &gt; Resource Compiler &gt; Directories and Conditionals &gt; Include file search path</em></li>
<pre><code>../rickloading/src</code></pre>

<h2>⚡️ How to use the project</h2>
<pre><code>../rickloading/src</code></pre>

<pre><span class="pl-k">uses</span>
  RICK.Loading;

<span class="pl-k">implementation</span>

begin
  TRICKLoading.New
    .Executar(
    procedure
    begin
      //Command
      Sleep(500);

      //Command to refresh the Canvas
      TThread.Synchronize(TThread.Current,
      procedure
      begin
        ShowMessage('Command to refresh the screen here...');
      end);
    end);
end;</span></pre>

<li><strong>Another way to use</strong>: <br>

<pre><span class="pl-k">uses</span>
  RICK.Loading;

<span class="pl-k">implementation</span>

begin
  TRICKLoading.New
    .Executar(
    procedure
    begin
      //Delayed Command
      Sleep(500);

      TThread.Synchronize(TThread.Current,
      procedure
      begin
        TRICKLoading.New
          .ChangeMessage('Changing message'); //Change the message to the user
      end);

      //Outro comando
      TThread.Sleep(1500);

      TThread.Synchronize(TThread.Current,
      procedure
      begin
        //Command to refresh the screen
        ShowMessage('Command to refresh the screen here...');
      end);
    end);
end;</span></pre>

<li><strong>Example using the Interface</strong>: <br>

<pre><span class="pl-k">uses</span>
  RICK.Loading.Interfaces,
  RICK.Loading;

<span class="pl-k">implementation</span>

var
  LRICKLoading: iRICKLoading;
begin
  LRICKLoading:= TRICKLoading.New;
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

end;</span></pre>
