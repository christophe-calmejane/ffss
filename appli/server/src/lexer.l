%{
#define YYSTYPE void *
#include "parser.h"
#include <string.h>
int yyerror (char *s);
extern void *yylval;
extern int line;
%}

%%

"LimitedBind"        { return BIND; }
"Plugin"             { return PLUGIN; }
"XFerBufferSize"     { return XFERSIZE; }
"ReadBufferSize"     { return READSIZE; }
"XFerInConn"         { return XFERCONN; }
"Interface"          { return INTNAME; }
"MyIP"               { return MYIP; }
"Debug"              { return DBG; }
"ConfSocket"         { return SOCK; }
"Idle"               { return IDLE; }
"FTP"                { return USEFTP; }
"Global"             { return GLOBAL; }
"End"                { return END; }
"Name"               { return NAME; }
"Comment"            { return COMMENT; }
"Master"             { return MASTER; }
"Path"               { return PATH; }
"Writeable"          { return WRITEABLE; }
"Private"            { return PRIVATE; }
"NoChksum"           { return NOCHKSUM; }
"MaxConnections"     { return MAXCONN; }
"MaxXFerConnections" { return MAXXFERCONN; }
"FTP_MaxConnections" { return FTP_MAXCONN; }
"Users"              { return USERS; }
"Filter"             { return FILTER; }
"udp"                { return FILTER_UDP; }
"tcp"                { return FILTER_TCP; }
"tcp_ftp"            { return FILTER_TCP_FTP; }
"accept"             { return FILTER_ACCEPT; }
"reject"             { return FILTER_REJECT; }
"default"            { return FILTER_DEFAULT; }


[A-Za-z][A-Za-z0-9_]* { yylval = (void *)strdup(yytext); return STRING; }
\"[^"]*              { if (yytext[yyleng-1] == '\\')
                         yymore();
                       else
                       {
                         input();
                         yylval = (void *)strdup(yytext+1);
                         return STRING;
                       }
                     }
[0-9]+               { yylval = (void *)atoi(yytext); return NUM; }
"<"                  { return INF; }
">"                  { return SUP; }
"{"                  { return L_BRACE; }
"}"                  { return R_BRACE; }
"("                  { return L_PAR; }
")"                  { return R_PAR; }
","                  { return VIRG; }
[ \t]                ;
[\n]                 { line++; }
"#"[^\n]*            { line++;input(); }
.                    { yyerror("Illegal token"); }

%%
#ifndef yywrap
int yywrap() { return 1; }
#endif

