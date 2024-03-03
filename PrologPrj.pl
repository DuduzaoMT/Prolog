% 106929, Eduardo Silva
:- set_prolog_flag(answer_write_options,[max_depth(0)]).%para listas completas
:- ['dados.pl'],['keywords.pl'].%ficheiros a importar.


% 3.1

% # eventosSemSalas/1 #

/*
   eventosSemSalas(EventosSemSala) e verdade se EventosSemSala e uma
   lista, ordenada e sem elementos repetidos, de IDs de eventos sem
   sala.
*/

eventosSemSalas(EventosSemSala):-
    setof(ID,Disc^Tipo^NumAlunos^evento(ID,Disc,Tipo,NumAlunos,semSala),EventosSemSala).

% # eventosSemSalasDiaSemana/2 #

/*
 eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) e verdade se
 EventosSemSala e uma lista, ordenada e sem elementos repetidos, de IDs
 de eventos sem sala que decorrem em DiaDaSemana.
*/

eventosSemSalasDiaSemana(DiaSemana,EventosSemSala):-
    findall(ID,(evento(ID,_,_,_,semSala),horario(ID,DiaSemana,_,_,_,_)),Eventos),
    sort(Eventos,EventosSemSala).

% factos sobre os periodos de forma a poder ser mais facil considerar
% eventos que occoram semestralmente.

periodos(p1,p1_2,p1).
periodos(p2,p2,p1_2).
periodos(p3,p3,p3_4).
periodos(p4,p4,p3_4).

% # eventosSemSalasPeriodo/3 #

/*
   eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) e verdade se
   ListaPeriodos e uma lista de periodos e EventosSemSala e uma lista,
   ordenada e sem elementos repetidos, de IDs de eventos sem sala nos
   periodos de ListaPeriodos.
*/

% Procura todo os IDs sem sala no periodo ou no semestre e adiciona.

eventosSemSalasPeriodo([],[]).
% Procuro periodo a periodo todos os eventos sem sala.
eventosSemSalasPeriodo([Periodo|Resto],EventosSemSala):-
    periodos(Periodo,Periodo1,Semestre),
    findall(ID,
            (evento(ID,_,_,_,semSala),(horario(ID,_,_,_,_,Periodo1);horario(ID,_,_,_,_,Semestre))),
            SemSala),
    eventosSemSalasPeriodo(Resto,Eventos),
    append(SemSala,Eventos,ListaSemSala),
    sort(ListaSemSala,EventosSemSala).

% 3.2

% # organizaEventos/3 #

/*
  organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) e verdade se
  EventosNoPeriodo e a lista, ordenada e sem elementos repetidos, de IDs
  dos eventos de ListaEventos que ocorrem no periodo passado como
  argumento.
*/

% verificar os IDS um a um vendo se verificavam as condicoes de periodo
% e adiciona-los, ordenadamente, a lista final atraves do sort.

organizaEventos([],_,[]).
organizaEventos([ID|Restantes_IDS],Periodo,EventosNoPeriodo):-
    periodos(Periodo,Periodo_N,Semestral),
    (horario(ID,_,_,_,_,Periodo_N);horario(ID,_,_,_,_,Semestral)),
    organizaEventos(Restantes_IDS,Periodo,Eventos_Periodo),
    sort([ID|Eventos_Periodo],EventosNoPeriodo).
organizaEventos([_|Restantes_IDS],Periodo,EventosNoPeriodo):-
    organizaEventos(Restantes_IDS,Periodo,EventosNoPeriodo).

% # eventosMenoresQue/2 #

/*
   eventosMenoresQue(Duracao, ListaEventosMenoresQue) e verdade se
   ListaEventosMenoresQue e a lista ordenada e sem elementos repetidos
   dos identificadores dos eventos que tem duracao menor ou igual a
   Duracao.
*/

% Todos os eventos com duracao menor que a passada no argumento.

eventosMenoresQue(Duracao,ListaEventosMenoresQue):-
    findall(ID,
            (horario(ID,_,_,_,DuracaoEvento,_),
             DuracaoEvento=<Duracao),
            ListaMenores),
    sort(ListaMenores,ListaEventosMenoresQue).

% # eventosMenroesQueBool/2 #

/*
   eventosMenoresQueBool(ID, Duracao) e verdade se o evento identificado
   por ID tiver duracao igual ou menor a Duracao.
*/

eventosMenoresQueBool(ID,Duracao):-
    horario(ID,_,_,_,DuracaoEvento,_),DuracaoEvento=<Duracao.

% # procuraDisciplinas/2 #

/* lista ordenada alfabeticamente do nome das disciplinas do curso Curso. */

procuraDisciplinas(Curso,ListaDisciplinas):-
    findall(Disc,
            (evento(ID,Disc,_,_,_),turno(ID,Curso,_,_)),
            Lista_Disciplinas),
    sort(Lista_Disciplinas,ListaDisciplinas).

% # organizaDisciplinas/3 #

/*
  organizaDisciplinas(ListaDisciplinas, Curso, Semestres), devolve uma
  lista de duas listas(Semestres) em que a primeira lista tem as
  disciplinas da ListaDsiciplinas(Dada como argumento) que pertence ao
  primeiro semestre do curso passado como arguemnto, respetivamente, a
  segunda lista contem as disciplinas de ListaDisciplinas que ocorrem no
  segundo semestre.
*/

organizaDisciplinas(ListaDisciplinas,Curso,Semestres):-organizaDisciplinasAux(ListaDisciplinas,Curso,[],[],Semestres).
% caso terminal onde da append das duas listas na lista final.
organizaDisciplinasAux([],_,ListaSemestre1,ListaSemestre2,ListaResultado):-append([ListaSemestre1],[ListaSemestre2],ListaResultado).
% ve cada uma das disciplinas e adiciona a uma ou outra lista.
organizaDisciplinasAux([Disc|RestoDisciplinas],Curso,ListaS1,ListaS2,Semestres):-
    turno(ID,Curso,_,_),
    evento(ID,Disc,_,_,_),
    (horario(ID,_,_,_,_,p1);horario(ID,_,_,_,_,p2);horario(ID,_,_,_,_,p1_2)), % ve se percente a algum dos periodos do primeiro semestre.
    append([Disc],ListaS1,Nova_ListaS1),
    sort(Nova_ListaS1,ListaS1Final),
    organizaDisciplinasAux(RestoDisciplinas,Curso,ListaS1Final,ListaS2,Semestres).
organizaDisciplinasAux([Disc|RestoDisciplinas],Curso,ListaS1,ListaS2,Semestres):-
    turno(ID,Curso,_,_),
    evento(ID,Disc,_,_,_),
    (horario(ID,_,_,_,_,p3);horario(ID,_,_,_,_,p4);horario(ID,_,_,_,_,p3_4)), % ve se pertence a algum dos periodos do segundo semestre.
    append([Disc],ListaS2,Nova_Lista2),
    sort(Nova_Lista2,ListaS2Final),
    organizaDisciplinasAux(RestoDisciplinas,Curso,ListaS1,ListaS2Final,Semestres).

%  # Predicado horasCurso/4 #

/*
   Este predicado devolve o total de horas de eventos associados de um
   determinado curso num determinado ano e periodo (dados como
   argumentos).
*/

%  agrupar todos os IDS de eventos que satisfazem as condicoes do
%  enunciado (curso, ano, periodo), eliminar repetidos atraves do sorte
%  e somar as respetivas duracoes.

horasCurso(Periodo,Curso,Ano,TotalHoras):-
    periodos(Periodo,Periodo_N,Semestral),
    findall(ID,
            (turno(ID,Curso,Ano,_),
             (horario(ID,_,_,_,Duracao,Periodo_N);horario(ID,_,_,_,Duracao,Semestral)),
             evento(ID,_,_,_,_)),
            ListaIDS),
    sort(ListaIDS,ListaSemRep),
    findall(Duracao,
            (member(ID,ListaSemRep),horario(ID,_,_,_,Duracao,_)),
            ListaHoras),
    sumlist(ListaHoras,TotalHoras).

%  # Predicado evolucaoHorasCurso/2 #

/*
    Predicado que devolve o numero de horas por ano e por periodo do
    curso pedido numa lista de tuplos de estrutura :
    (Ano,Periodo,NumHoras).
*/

% A versao anterior nao considerava os 3 anos a priori, considerava
% apenas os anos de existencia do curso, no entanto, nao passava nos
% testes.

evolucaoHorasCurso(Curso,Evolucao):-
    findall((Ano,Periodo,NumHoras),
            (member(Ano,[1,2,3]),member(Periodo,[p1,p2,p3,p4]),horasCurso(Periodo,Curso,Ano,NumHoras)),
            ListaTuplos),
    sort(ListaTuplos,Evolucao).

% 3.3

% # ocupaSlot/5 #

/*
   Devolve as horas de colisao entre dois eventos. Se nao houver
   intersecao devolve False.
*/

% Aquando deste intersecao, ha uma regra que se repete, a intersecao e a
% diferenca entre o min dos finais menos o max dos inicios.

ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas):-
    Superior is min(HoraFimEvento,HoraFimDada),
    Inferior is max(HoraInicioEvento,HoraInicioDada),
    Horas is Superior - Inferior,
    Horas>=0.

% # NumHorasOcupadas/6 #

/*
   Devolve o numero de horas ocupadas naquele periodo de tempo (entre
   HoraInicio e HoraFim),no tipo de sala referido.
*/

% Utilizacao do corte para garantir que a solucao seja unica e
% possibilite a utilizacao deste mesmo predicado no ultimo predicado
% desta seccao.

numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras):-
    salas(TipoSala,ListaSala),!,
    somaHorasAux(Periodo,ListaSala,DiaSemana,HoraInicio,HoraFim,SomaHoras).

% Utilizacao do ocupaslot para calcular o periodo de intercecao entre o
% periodo que decorre a aula e o periodo enunciado no predicado
% inicial(NumHorasOcupadas).

somaHorasAux(_,[],_,_,_,0):-!.
somaHorasAux(Periodo,[Sala|RestantesSalas],DiaSemana,HoraInicio,HoraFim,SomaHoras):-
    periodos(Periodo,Periodo_N,Semestral),
    findall(HorasEfetivas,
            (evento(ID,_,_,_,Sala),
             (horario(ID, DiaSemana, HoraInicioAula, HoraFimAula ,_, Periodo_N);horario(ID, DiaSemana, HoraInicioAula, HoraFimAula ,_, Semestral)),
             ocupaSlot(HoraInicio,HoraFim,HoraInicioAula,HoraFimAula,HorasEfetivas)),
            ListaHorasSala),
    sumlist(ListaHorasSala,HorasSala),!, 
    somaHorasAux(Periodo,RestantesSalas,DiaSemana,HoraInicio,HoraFim,ContadorDeHoras),
    SomaHoras is HorasSala+ContadorDeHoras. % adicao de todas as horas ocupadas de todas as salas.
% descobrir todas as duracoes, ver as que coincidem com o periodo de tempo dado e adicionar a lista para as somar.
somaHorasAux(Periodo,[_|RestantesSalas],DiaSemana,HoraInicio,HoraFim,SomaHoras):-
    !,somaHorasAux(Periodo,RestantesSalas,DiaSemana,HoraInicio,HoraFim,SomaHoras).

% # ocupacaoMax/4 #

/*
   Devolve o numero de horas maximo de ocupacao daquele tipo de Sala.
*/

ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max):-
    salas(TipoSala,ListaSalas),
    length(ListaSalas,N_Salas),
    Max is (HoraFim-HoraInicio)*N_Salas.

% # percentagem/3 #

/*
  Devolve a percentagem de ocupacao das salas.
*/

percentagem(SomaHoras,Max,Percentagem):-
    Percentagem is (SomaHoras/Max)*100.

% # ocupacaoCritica/4 #

/*
   Devolve os tipos de sala que se encontram num ponto critico de
   ocupacao, este valor critico e dado num dos argumentos do predicado.
*/

% lista de casos criticos de ocupacao.
% Utiliza os predicados definidos em cima.

ocupacaoCritica(HoraInicio,HoraFim,Threshold,ResultadosSemReps):-
    findall(casosCriticos(DiaSemana,TipoSala,PercentagemArredondada),
            (member(DiaSemana,[segunda-feira,terca-feira,quarta-feira,quinta-feira,sexta-feira]),
             findall(TipoSala,(salas(TipoSala,_)),ListaDeTiposSalas),
             member(TipoSala,ListaDeTiposSalas),member(Periodo,[p1,p2,p3,p4]),
             numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras),
             ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max),
             percentagem(SomaHoras,Max,Percentagem),
             Percentagem>Threshold,
             ceiling(Percentagem,PercentagemArredondada)),
            Resultados),
    sort(Resultados,ResultadosSemReps).

% 3.4

/*
   ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) e verdade
   se ListaPessoas for a lista com o nome das pessoas a sentar a mesa,
   ListaRestricoes for a lista de restricoes a verificar e
   OcupacaoMesa for uma lista com tres listas, em que a primeira contem
   as pessoas de um lado da mesa, a segunda as pessoas a
   cabeceira,e a terceira as pessoas do outro lado da mesa , de modo a
   que essas pessoas sao exactamente as da ListaPessoas e verificam
   todas as restricoes de ListaRestricoes.
*/

% ocupacao correta da mesa passa por pegar numa permutacao do predicado
% permutation, verificar se essa ocupacao e valida para aquelas
% restricoes e se for apresentar essa mesa como resultado, se nao pegar
% numa nova permutacao e verificar as condicoes novamente.

ocupacaoMesa(ListaPessoas,ListaCondicoes,MesaOrganizada):-
    permutation(ListaPessoas,[X1,X2,X3,X4,X5,X6,X7,X8]),
    MesaOrganizada = [[X1,X2,X3],[X4,X5],[X6,X7,X8]],
    verificaCondicoesOrganizacao(ListaCondicoes,MesaOrganizada).
% predicado auxiliar que passa por todas as restricoes da lista
verificaCondicoesOrganizacao([],_).
verificaCondicoesOrganizacao([Restricao|ListaCondicoes],MesaOrganizada):-
    auxiliarVerificacaoRestricoes(Restricao,MesaOrganizada),!,
    verificaCondicoesOrganizacao(ListaCondicoes,MesaOrganizada).
% todas as restricoes possiveis da mesa
% cabeiceira
auxiliarVerificacaoRestricoes(cab1(NomePessoa),[[_,_,_],[NomePessoa,_],[_,_,_]]).
auxiliarVerificacaoRestricoes(cab2(NomePessoa),[[_,_,_],[_,NomePessoa],[_,_,_]]).
% honra
auxiliarVerificacaoRestricoes(honra(NomePessoa1,NomePessoa2),[[_,_,NomePessoa2],[_,NomePessoa1],[_,_,_]]).
auxiliarVerificacaoRestricoes(honra(NomePessoa1,NomePessoa2),[[_,_,_],[NomePessoa1,_],[NomePessoa2,_,_]]).
% lado
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[NomePessoa1,NomePessoa2,_],[_,_],[_,_,_]]).
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[NomePessoa2,NomePessoa1,_],[_,_],[_,_,_]]).
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[_,NomePessoa1,NomePessoa2],[_,_],[_,_,_]]).
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[_,NomePessoa2,NomePessoa1],[_,_],[_,_,_]]).
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[_,_,_],[_,_],[NomePessoa1,NomePessoa2,_]]).
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[_,_,_],[_,_],[NomePessoa2,NomePessoa1,_]]).
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[_,_,_],[_,_],[_,NomePessoa1,NomePessoa2]]).
auxiliarVerificacaoRestricoes(lado(NomePessoa1,NomePessoa2),[[_,_,_],[_,_],[_,NomePessoa2,NomePessoa1]]).
% nao lado - Fiz atraves de functores porque nao estava a funcionar com negacao.
auxiliarVerificacaoRestricoes(naoLado(NomePessoa1,NomePessoa2),Mesa):-
    Lado =..[auxiliarVerificacaoRestricoes,lado(NomePessoa1,NomePessoa2),Mesa],\+ Lado.
% frente
auxiliarVerificacaoRestricoes(frente(P1,P2),[[P1,_,_],[_,_],[P2,_,_]]).
auxiliarVerificacaoRestricoes(frente(P1,P2),[[P2,_,_],[_,_],[P1,_,_]]).
auxiliarVerificacaoRestricoes(frente(P1,P2),[[_,P1,_],[_,_],[_,P2,_]]).
auxiliarVerificacaoRestricoes(frente(P1,P2),[[_,P2,_],[_,_],[_,P1,_]]).
auxiliarVerificacaoRestricoes(frente(P1,P2),[[_,_,P1],[_,_],[_,_,P2]]).
auxiliarVerificacaoRestricoes(frente(P1,P2),[[_,_,P2],[_,_],[_,_,P1]]).
% nao frente - Fiz atraves de functores porque nao estava a funcionar com negacao.
auxiliarVerificacaoRestricoes(naoFrente(P1,P2),Mesa):-
    Frente =..[auxiliarVerificacaoRestricoes,frente(P1,P2),Mesa],\+Frente.