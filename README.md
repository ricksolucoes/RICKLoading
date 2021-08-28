# RI2BCarregando

<p align="center">
  <b>RI2BCarregando</b> é uma biblioteca Delphi desenvolvida para apresentar tela de carregando. Utilizando o Fluent Interface do delphi.
</p>

<h2>⚙️ Instalação</h2>
<p>*Pré requisitos: Delphi FMX</p>
<li><strong>Instalação com BOSS</strong>: <br>
<pre>$ boss install https://github.com/ricksolucoes/RI2BCarregando</pre>

<li><strong>Instalação manual</strong>: <br>
Adicione as seguintes pastas ao seu projeto, em <em>Project &gt; Options &gt; Resource Compiler &gt; Directories and Conditionals &gt; Include file search path</em></li>
<pre><code>../RI2BCarregando/src</code></pre>

<h2>⚡️ Como utilizar para realizar o carregamento da animação</h2>

<pre><span class="pl-k">uses</span>
  RI2BCarregar.Registrar;

<span class="pl-k">implementation</span>

begin
  TCarregar.New
    .Executar(
    procedure
    begin
      //Comando
      Sleep(500);

      //Comando para atualizar a Tela
      TThread.Synchronize(TThread.Current,
      procedure
      begin
        ShowMessage('Comando para atualizar a tela aqui..');
      end);
    end);
end;</span></pre>

<li><strong>Outra forma de uso</strong>: <br>
  
<pre><span class="pl-k">uses</span>
  RI2BCarregar.Registrar;

<span class="pl-k">implementation</span>

begin
  TCarregar.New
    .Executar(
    procedure
    begin
      //Comando Demorado
      Sleep(500);

      TThread.Synchronize(TThread.Current,
      procedure
      begin
        TCarregar.New
          .MudarMensagem('Modificando mensagem'); //Modificar a mensagem para o usuario
      end);

      //Outro comando
      TThread.Sleep(1500);

      TThread.Synchronize(TThread.Current,
      procedure
      begin
        //Comando para atualizar a tela
        ShowMessage('Comando para atualizar a tela aqui..');
      end);
    end);
end;</span></pre>

<li><strong>Exmplo utilizando a Interface</strong>: <br>
  
<pre><span class="pl-k">uses</span>
  RI2BCarregar.Registrar;

<span class="pl-k">implementation</span>

var
  LCarrregar: iCarregar;
begin
  LCarrregar:= TCarregar.New;
  LCarrregar.Mensagem('Carregando Modificado'); //Muda a mensagem inicial do carregamento
  LCarrregar.TamanhoFonte(32); //Muda o tamanho da fonte
  LCarrregar.NomeFonte('Segoe UI'); //Muda o tipo da fonte
  LCarrregar.CorFonte($FFF52121); //Muda a cor da fonte
  LCarrregar.CorAnimacao($FFF52121); //Muda a cor da animação
  LCarrregar.CorFundo($FF24CCC6); //Muda a cor do fundo do carregando
  LCarrregar.OpacidadeFundo(0.9); //Muda a opacidade do Fundo;
  LCarrregar.OpacidadeAnimacaoTexto(0.6); //Muda a opacidade do texto


  LCarrregar.Executar(
  procedure
  begin

    TThread.Sleep(500);

    TThread.Synchronize(TThread.Current,
    procedure
    begin
      LCarrregar.MudarMensagem('Modificando mensagem'); //Modificar a mensagem para o usuario
    end);

    TThread.Sleep(1500);


    TThread.Synchronize(TThread.Current,
    procedure
    begin
      ShowMessage('Comando para atualizar a tela aqui..');
    end);
  end);

end;</span></pre>
