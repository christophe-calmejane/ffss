/* FFSS server - Config file parser */

%{
#define YYSTYPE void *
typedef unsigned int bool;
#define true 1
#define false 0
#include <stdio.h>
#include "server.h"
FS_TShare TmpShr;
int line;
extern FILE *yyin;
int yyerror (char *s);

void FS_FreeTempoShare(void)
{
  if(TmpShr.Path != NULL)
    free(TmpShr.Path);
  if(TmpShr.Comment != NULL)
    free(TmpShr.Comment);
  memset(&TmpShr,0,sizeof(TmpShr));
}

void FS_MakeTempoUser(const char Login[],const char Password[],bool Writeable)
{
  FS_PUser Usr;

  Usr = (FS_PUser) malloc(sizeof(FS_TUser));
  memset(Usr,0,sizeof(FS_TUser));
  Usr->Login = strdup(Login);
  Usr->Password = strdup(Password);
  Usr->Writeable = Writeable;
  TmpShr.Users = SU_AddElementHead(TmpShr.Users,Usr);
}

%}

%token PLUGIN
%token XFERSIZE
%token READSIZE
%token XFERCONN
%token INTNAME
%token MYIP
%token DBG
%token SOCK
%token IDLE
%token USEFTP
%token NUM
%token STRING
%token INF
%token SUP
%token L_BRACE
%token R_BRACE
%token L_PAR
%token R_PAR
%token VIRG
%token GLOBAL
%token END
%token NAME
%token COMMENT
%token MASTER
%token PATH
%token WRITEABLE
%token PRIVATE
%token MAXCONN
%token MAXXFERCONN
%token FTP_MAXCONN
%token USERS

%% /* Grammar rules and actions follow */

input:    field
        | input field
;

field:    INF GLOBAL SUP fieldcontentglobal INF END SUP
        | INF STRING SUP fieldcontent INF END SUP            { FS_BuildIndex(TmpShr.Path,$2,TmpShr.Comment,TmpShr.Writeable,TmpShr.Private,TmpShr.MaxConnections,TmpShr.Users,false); FS_FreeTempoShare(); }
;

fieldcontentglobal:   fieldlineglobal
              | fieldcontentglobal fieldlineglobal
;

fieldcontent:   fieldline
              | fieldcontent fieldline
;

fieldlineglobal:   NAME STRING                               { FS_MyGlobal.Name = strdup($2); }
                 | COMMENT STRING                            { FS_MyGlobal.Comment = strdup($2); }
                 | MASTER STRING                             { FS_MyGlobal.Master = strdup($2); }
                 | IDLE NUM                                  { FS_MyGlobal.Idle = (int)($2); }
                 | MAXCONN NUM                               { FS_MyGlobal.MaxConn = (int)($2); }
                 | MAXXFERCONN NUM                           { FS_MyGlobal.MaxXFerPerConn = (int)($2); }
                 | USEFTP NUM                                { FS_MyGlobal.FTP = (int)($2); }
                 | FTP_MAXCONN NUM                           { FS_MyGlobal.FTPMaxConn = (int)($2); }
                 | SOCK NUM                                  { FS_MyGlobal.ConfSock = (int)($2); }
                 | XFERCONN NUM                              { FS_MyGlobal.XFerInConn = (int)($2); }
                 | MYIP STRING                               { FS_MyGlobal.MyIP = strdup($2); }
                 | INTNAME STRING                            { FS_MyIntName = strdup($2); }
                 | READSIZE NUM                              { FFSS_TransferReadBufferSize = (long int)($2); }
                 | XFERSIZE NUM                              { FFSS_TransferBufferSize = (long int)($2); }
                 | PLUGIN STRING                             { FS_LoadPlugin($2); }
                 | DBG NUM                                   { N_DebugLevel = (int)($2); }
;

fieldline:         PATH STRING                               { TmpShr.Path = strdup($2); }
                 | COMMENT STRING                            { TmpShr.Comment = strdup($2); }
                 | WRITEABLE NUM                             { TmpShr.Writeable = (int)($2); }
                 | PRIVATE NUM                               { TmpShr.Private = (int)($2); }
                 | MAXCONN NUM                               { TmpShr.MaxConnections = (int)($2); }
                 | USERS users
;

users:   L_BRACE usersrec R_BRACE
;

usersrec:   user
          | usersrec VIRG user
;

user: L_PAR STRING VIRG STRING VIRG NUM R_PAR                { FS_MakeTempoUser($2,$4,(int)$6); }
;

%%


int yyerror (char *s)
{
  FFSS_PrintSyslog(LOG_ERR,"Config file parser error at line %d (%s)\n",line,s);
  return -1;
}

bool FS_LoadConfig(const char FileName[])
{
  memset(&TmpShr,0,sizeof(TmpShr));
  yyin = fopen(FileName, "rt");
  if(yyin == NULL)
  {
    FFSS_PrintSyslog(LOG_ERR,"Can't open config file : %s\n",FileName);
    return false;
  }
  line = 1;
  FS_MyGlobal.MaxConn = FFSS_DEFAULT_MAX_CONN;
  FS_MyGlobal.MaxXFerPerConn = FFSS_DEFAULT_MAX_XFER_PER_CONN;
  FS_MyGlobal.Idle = 60*5;
  return (yyparse() == 0);
}
