/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	display functions
 */

#ifndef _FCA_DISPLAY_H_
#define _FCA_DISPLAY_H_

#include <skyutils.h>
#include <ffss.h>

extern char *FCA_dw_file;
extern FFSS_LongField FCA_dw_amount;
extern FFSS_LongField FCA_dw_size;
extern time_t FCA_dw_begin;
extern int FCA_nb_states;
extern FILE *FCA_err_stream;

bool FCA_question(const char *question);

void FCA_print_state(long int State,const char *IP,const char *Domain,const char *Name,const char *OS,const char *Comment,const char *MasterIP);
void FCA_print_post_states();
void FCA_print_servers(const char *Domain, const int NbHost,SU_PList HostList);
void FCA_print_ls(const char Path[],int NbEntries,SU_PList Entries);
void FCA_print_shares(const char IP[],const char **Names,const char **Comments,int NbShares);
void FCA_print_domains(const char **Domains, int NbDomains);
void FCA_print_search(const char *Query,const char *Domain,const char **Answers,int NbAnswers);

void FCA_print_prog_bar();
bool FCA_print_pwd(char *args);
bool FCA_print_lpwd(char *args);

void FCA_print_help(void);
void FCA_version(void);
void FCA_print_version(void);

void FCA_print_env();
void FCA_print_var(int index);

#ifdef ALIASES
void FCA_print_aliases(char *cmd);
#endif

void FCA_print_info(char *Txt, ...);
void FCA_print_status(char *name, bool value);
void FCA_print_cmd_ok(char *msg, ...);

void FCA_print_conn_err(SU_PClientSocket Server,int Code,const char Descr[]);
void FCA_print_arg_err(unsigned short int iarg);
void FCA_print_err(char *msg, ...);
void FCA_print_cmd_err(char *msg, ...);
void FCA_print_warning(char *msg, ...);
void FCA_crash(char *msg, ...);

/*************** debug *****************/
bool FCA_debug_struct(char *args);
void FCA_print_dbg_info();

void FCA_print_cmd_help(char *cmd);

void FCA_print_size(FFSS_LongField size, char *format);
void FCA_print_nb(int nb, const char name[]);

void FCA_ansi_chs(unsigned short int style);
void FCA_ansi_chs_err(unsigned short int style);
void FCA_ansi_clrl();

#endif	/* _FCA_DISPLAY_H_ */
