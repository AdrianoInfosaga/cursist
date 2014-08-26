CREATE TABLE appl_grupoprogramas (
    id            INTEGER        PRIMARY KEY NOT NULL,
    nome          VARCHAR( 20 )  NOT NULL,
    dtcadastro    DATE           NOT NULL,
    dtmodificacao DATE );

CREATE TABLE appl_usuarios (
    id            INTEGER     PRIMARY KEY,
    nome          CHAR( 20 )  NOT NULL  COLLATE 'NOCASE',
    senha         CHAR( 5 )   NOT NULL  COLLATE 'NOCASE',
    perfil        CHAR( 1 )   NOT NULL,
    dtcadastro    DATE        NOT NULL,
    dtmodificacao DATE,
    tiporegistro  INTEGER );

INSERT INTO [appl_usuarios] ([id], [nome], [senha], [perfil], [dtcadastro], [dtmodificacao], [tiporegistro]) VALUES (1, 'MASTER', 'MASTER', 'A', 20142108, 20142108, 'F');
INSERT INTO [appl_usuarios] ([id], [nome], [senha], [perfil], [dtcadastro], [dtmodificacao], [tiporegistro]) VALUES (2, 'ROOT', 'ROOT', 'S', 20142108, 20142108, 'F');

CREATE TABLE appl_programas (
    id                INTEGER        PRIMARY KEY NOT NULL,
    id_grupoprogramas INTEGER        REFERENCES appl_grupoprogramas ( id ),
    descr             VARCHAR( 50 )  COLLATE 'NOCASE',
    parametros        TEXT,
    executavel        VARCHAR( 20 )  NOT NULL,
    categoria         CHAR( 1 )      NOT NULL,
    dtcadastro        DATE           NOT NULL,
    perfil            CHAR( 1 )      NOT NULL,
    dtmodificacao     DATE );

CREATE TABLE usr_tipoavaliacao (
    id            INTEGER        PRIMARY KEY  NOT NULL,
    peso          INTEGER        NOT NULL,
    descr         VARCHAR( 20 )  NOT NULL COLLATE 'NOCASE',
    obs           TEXT,
    dtcadastro    DATE           NOT NULL,
    dtmodificacao DATE );

CREATE TABLE usr_aluno (
    id            INTEGER        PRIMARY KEY NOT NULL,
    nome          VARCHAR( 40 )  NOT NULL COLLATE 'NOCASE',
    nasc          DATE,
    endereco      VARCHAR( 40 ),
    numero        VARCHAR( 5 ),
    compl         VARCHAR( 10 ),
    bairro        VARCHAR( 20 ),
    cidade        VARCHAR( 30 ),
    uf            VARCHAR( 2 ),
    cep           VARCHAR( 8 ),
    foto          BLOB,
    telefones     VARCHAR( 40 ),
    email         VARCHAR( 40 ),
    dtcadastro    DATE           NOT NULL,
    obs           TEXT,
    dtmodificacao DATE );

CREATE TABLE usr_curso (
    id             INTEGER          PRIMARY KEY NOT NULL,
    descr          VARCHAR( 40 )    NOT NULL  COLLATE 'NOCASE',
    qtmodulos      INTEGER          NOT NULL  DEFAULT ( 9 ),
    mediaaprovacao NUMERIC( 6, 2 ),
    obs            TEXT,
    dtcadastro     DATE             NOT NULL,
    dtmodificacao  DATE );

CREATE TABLE usr_instituicao (
    nome          CHAR( 40 )     NOT NULL COLLATE 'NOCASE',
    rua           VARCHAR( 40 ),
    numero        VARCHAR( 5 ),
    compl         VARCHAR( 10 ),
    bairro        VARCHAR( 20 ),
    cidade        VARCHAR( 30 ),
    uf            CHAR( 2 ),
    cep           CHAR( 8 ),
    logo          BLOB,
    dtcadastro    DATE           NOT NULL,
    id            INTEGER        NOT NULL PRIMARY KEY,
    dtmodificacao DATE );

CREATE TABLE usr_turma (
    id            INTEGER        PRIMARY KEY NOT NULL,
    id_curso      INTEGER        NOT NULL  REFERENCES usr_curso ( id ),
    descr         VARCHAR( 40 )  NOT NULL  COLLATE 'NOCASE',
    dtinicio      DATE           NOT NULL,
    dtfinal       DATE,
    indsit        CHAR( 1 ),
    dtcadastro    DATE           NOT NULL,
    dtmodificacao DATE );

CREATE TABLE usr_matricula (
    id            INTEGER    PRIMARY KEY NOT NULL,
    id_turma      INTEGER    NOT NULL  REFERENCES usr_turma ( id ),
    id_aluno      INTEGER    NOT NULL  REFERENCES usr_aluno ( id ),
    dtmatricula   DATE       NOT NULL,
    obs           TEXT,
    indsit        CHAR( 1 )  NOT NULL,
    dtcadastro    DATE       NOT NULL,
    dtmodificacao DATE );

CREATE TABLE usr_notasobtidas (
    id               INTEGER          PRIMARY KEY NOT NULL,
    id_matricula     INTEGER          NOT NULL REFERENCES usr_matricula ( id ),
    id_tipoavaliacao INTEGER       NOT NULL REFERENCES usr_tipoavaliacao ( id ),
    dataavaliacao    DATE             NOT NULL,
    notaobtida       NUMERIC( 6, 2 ),
    obs              TEXT,
    dtcadastro       DATE             NOT NULL,
    dtmodificacao    DATE );

CREATE TABLE appl_acessoprogramas (
    id            INTEGER    PRIMARY KEY NOT NULL,
    id_programa   INTEGER    NOT NULL  REFERENCES appl_programas ( id ),
    id_usuario    INTEGER    NOT NULL  REFERENCES appl_usuarios ( id ),
    nivelacesso   CHAR( 1 )  NOT NULL,
    dtmodificacao DATE );

CREATE TABLE appl_datalog (
    id            INTEGER        PRIMARY KEY NOT NULL,
    id_usuario    INTEGER        NOT NULL  REFERENCES appl_usuarios ( id ),
    datahora      DATETIME       NOT NULL,
    acao          CHAR( 1 )      NOT NULL
                                 COLLATE 'NOCASE',
    modulo        VARCHAR( 40 )  NOT NULL
                                 COLLATE 'NOCASE',
    tabela        VARCHAR( 40 )  COLLATE 'NOCASE',
    id_tabela     INTEGER,
    detalhes      TEXT           NOT NULL,
    dtmodificacao DATE );

CREATE TABLE appl_sequencias (
    nometabela    CHAR( 15 )  NOT NULL,
    posicao       INTEGER     NOT NULL,
    id            INTEGER     PRIMARY KEY NOT NULL,
    dtcadastro    DATE,
    dtmodificacao DATE );

CREATE UNIQUE INDEX idx_curso ON usr_curso ( id );
CREATE INDEX idx_curso2 ON usr_curso ( descr );
CREATE INDEX idx_turma ON usr_turma ( id );
CREATE INDEX idx_turma2 ON usr_turma ( id_curso );
CREATE INDEX idx_turma4 ON usr_turma ( indsit, dtinicio );
CREATE INDEX idx_turma3 ON usr_turma ( descr );
CREATE INDEX idx_matricula ON usr_matricula ( id );
CREATE INDEX idx_matricula2 ON usr_matricula ( id_turma );
CREATE INDEX idx_matricula3 ON usr_matricula ( id_aluno );
CREATE INDEX idx_matricula4 ON usr_matricula ( dtmatricula );
CREATE INDEX idx_aluno2 ON usr_aluno ( nasc );
CREATE INDEX idx_aluno3 ON usr_aluno ( nome );
CREATE INDEX idx_aluno ON usr_aluno ( id );
CREATE INDEX idx_avaliacao2 ON usr_notasobtidas ( id_matricula );
CREATE INDEX idx_avaliacao ON usr_notasobtidas ( id );
CREATE INDEX idx_avaliacao3 ON usr_notasobtidas ( dataavaliacao );
CREATE INDEX idx_tipos_avaliacao ON usr_tipoavaliacao ( id );
CREATE INDEX idx_tipos_avaliacao2 ON usr_tipoavaliacao ( descr );
CREATE INDEX idx_appl_acessoprogramas ON appl_acessoprogramas ( id );
CREATE INDEX idx_appl_acessoprogramas2 ON appl_acessoprogramas ( id_programa );
CREATE INDEX idx_appl_acessoprogramas3 ON appl_acessoprogramas ( id_usuario );
CREATE INDEX idx_appl_programas ON appl_programas ( id );
CREATE INDEX idx_appl_programas2 ON appl_programas ( descr );
CREATE INDEX idx_appl_grupoprogramas ON appl_grupoprogramas ( id );
CREATE INDEX idx_appl_grupoprogramas2 ON appl_grupoprogramas ( nome );
CREATE INDEX idx_appl_datalog ON appl_datalog ( id );
CREATE INDEX idx_appl_datalog2 ON appl_datalog ( id_usuario, datahora );
CREATE INDEX idx_appl_datalog3 ON appl_datalog ( modulo, datahora );
CREATE INDEX idx_sequencias ON appl_sequencias ( nometabela );
CREATE INDEX idx_appl_usuarios ON appl_usuarios ( id );
CREATE INDEX idx_appl_usuarios2 ON appl_usuarios ( nome );
CREATE INDEX idx_appl_usuarios3 ON appl_usuarios ( senha );

