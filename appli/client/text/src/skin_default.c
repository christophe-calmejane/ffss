/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	default skin
 */

#include <stdarg.h>
#include <ffss.h>

#include "display.h"
#include "skin.h"
#include "client.h"

int FCA_progr;

void FCA_def_tab_top();
void FCA_def_tab_title(const char title[]);
void FCA_def_tab_untitle();

void FCA_def_tab_pre_stitle();
void FCA_def_tab_stitle(const char name[], const unsigned int size);
void FCA_def_tab_int_stitle();
void FCA_def_tab_post_stitle();

void FCA_def_tab_pre_bar();
void FCA_def_tab_bar(const unsigned int size);
void FCA_def_tab_int_bar();
void FCA_def_tab_post_bar();

void FCA_def_tab_pre_item();
void FCA_def_tab_item(const char name[], const unsigned int size);
void FCA_def_tab_int_item();
void FCA_def_tab_post_item();

void FCA_def_tab_btm();

void FCA_def_pre_infos();
void FCA_def_infos(const char format[], ...);
void FCA_def_num(int n, const char text[]);
void FCA_def_info_size(const FFSS_LongField n, const char text[]);
void FCA_def_main_num(int n, const char text[]);
void FCA_def_post_infos();

void FCA_def_size(const FFSS_LongField n, const char text[]);

void FCA_def_pre_err();
void FCA_def_post_err();

void FCA_def_pre_warning();
void FCA_def_post_warning();

void FCA_def_pre_ok();
void FCA_def_post_ok();

void FCA_def_pre_serv(const char *domain, const char *name, long int state, bool isName);
void FCA_def_post_serv(bool isName);

void FCA_def_pre_dir(const char *prefx, const char *name, bool isName);
void FCA_def_post_dir(bool isName);

void FCA_def_pre_file(const char *prefx, const char *name, bool isName);
void FCA_def_post_file(bool isName);

bool FCA_def_pre_path(const char *domain, const char *path, long int state, bool isName, bool isSamba, bool isDir);
void FCA_def_post_path(bool isName);

void FCA_def_pre_file_exec(const char *prefx, const char *name, bool isName);
void FCA_def_post_file_exec(bool isName);

void FCA_def_progr_bar();
void FCA_def_dw_ok(char *file, float rate);


FCA_Tps FCA_def_ps;

void FCA_default_init()
{
	FCA_def_ps.tab_top=FCA_def_tab_top;
	FCA_def_ps.tab_title=FCA_def_tab_title;
	FCA_def_ps.tab_untitle=FCA_def_tab_untitle;
	
	FCA_def_ps.tab_pre_stitle=FCA_def_tab_pre_stitle;
	FCA_def_ps.tab_stitle=FCA_def_tab_stitle;
	FCA_def_ps.tab_int_stitle=FCA_def_tab_int_stitle;
	FCA_def_ps.tab_post_stitle=FCA_def_tab_post_stitle;
	
	FCA_def_ps.tab_pre_bar=FCA_def_tab_pre_bar;
	FCA_def_ps.tab_bar=FCA_def_tab_bar;
	FCA_def_ps.tab_int_bar=FCA_def_tab_int_bar;
	FCA_def_ps.tab_post_bar=FCA_def_tab_post_bar;
	
	FCA_def_ps.tab_pre_item=FCA_def_tab_pre_item;
	FCA_def_ps.tab_item=FCA_def_tab_item;
	FCA_def_ps.tab_int_item=FCA_def_tab_int_item;
	FCA_def_ps.tab_post_item=FCA_def_tab_post_item;
	
	FCA_def_ps.tab_btm=FCA_def_tab_btm;
	
	FCA_def_ps.pre_infos=FCA_def_pre_infos;
	FCA_def_ps.infos=FCA_def_infos;
	FCA_def_ps.num=FCA_def_num;
	FCA_def_ps.info_size=FCA_def_info_size;
	FCA_def_ps.main_num=FCA_def_main_num;
	FCA_def_ps.post_infos=FCA_def_post_infos;
	
	FCA_def_ps.size=FCA_def_size;
	
	FCA_def_ps.pre_err=FCA_def_pre_err;
	FCA_def_ps.post_err=FCA_def_post_err;

	FCA_def_ps.pre_warning=FCA_def_pre_warning;
	FCA_def_ps.post_warning=FCA_def_post_warning;

	FCA_def_ps.pre_ok=FCA_def_pre_ok;
	FCA_def_ps.post_ok=FCA_def_post_ok;

	FCA_def_ps.pre_serv=FCA_def_pre_serv;
	FCA_def_ps.post_serv=FCA_def_post_serv;

	FCA_def_ps.pre_dir=FCA_def_pre_dir;
	FCA_def_ps.post_dir=FCA_def_post_dir;

	FCA_def_ps.pre_file=FCA_def_pre_file;
	FCA_def_ps.post_file=FCA_def_post_file;
	
	FCA_def_ps.pre_path=FCA_def_pre_path;
	FCA_def_ps.post_path=FCA_def_post_path;
	
	FCA_def_ps.pre_file_exec=FCA_def_pre_file_exec;
	FCA_def_ps.post_file_exec=FCA_def_post_file_exec;
	
	FCA_def_ps.progr_bar=FCA_def_progr_bar;
	FCA_def_ps.dw_ok=FCA_def_dw_ok;
}


void FCA_def_tab_top()
{
	int i;
	
	FCA_ansi_chs(32);FCA_ansi_chs(1);
	for(i=0; i<FCA_tab_width; i++)
		printf("_");
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_tab_title(const char title[])
{
	char tmp[64];
	
	FCA_ansi_chs(42);printf(" ");FCA_ansi_chs(0);
	FCA_ansi_chs(44);
	snprintf(tmp,63," %%-%d.%ds", FCA_tab_width-2, FCA_tab_width-2);
	FCA_ansi_chs(37);printf(tmp, title);
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_tab_untitle()
{
	int i;
	
	FCA_ansi_chs(42);printf(" ");FCA_ansi_chs(0);
	FCA_ansi_chs(44);FCA_ansi_chs(32);FCA_ansi_chs(1);
	for(i=0; i<FCA_tab_width-1; i++)
		printf("_");
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_tab_pre_stitle()
{
	FCA_ansi_chs(42);printf(" ");FCA_ansi_chs(0);
	FCA_ansi_chs(44);
}

void FCA_def_tab_stitle(const char name[], const unsigned int size)
{
	char format[256];
	
	if(size) {
		snprintf(format, 256, " %%%d.%ds", size, size);
		printf(format,name);
	} else
		printf(" %s ", name);
}

void FCA_def_tab_int_stitle()
{
	printf("  ");
}

void FCA_def_tab_post_stitle()
{
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_tab_pre_bar()
{
	FCA_ansi_chs(42);printf(" ");FCA_ansi_chs(0);
	FCA_ansi_chs(0);
}

void FCA_def_tab_bar(const unsigned int size)
{
	int i;
	
	FCA_ansi_chs(30);FCA_ansi_chs(1);
	printf(" ");
	for(i=0; i<size; i++)
		printf("-");
}

void FCA_def_tab_int_bar()
{
	printf(" ");
	FCA_ansi_chs(44);printf(" ");FCA_ansi_chs(0);
}

void FCA_def_tab_post_bar()
{
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_tab_pre_item()
{
	FCA_ansi_chs(42);printf(" ");FCA_ansi_chs(0);
}

void FCA_def_tab_item(const char name[], const unsigned int size)
{
	char format[256];
	
	if(size) {
		snprintf(format, 256, " %%%d.%ds", size, size);
		printf(format,name);
	} else
		printf(" %s", name);
}

void FCA_def_tab_int_item()
{
	printf(" ");
	FCA_ansi_chs(44);printf(" ");FCA_ansi_chs(0);
}

void FCA_def_tab_post_item()
{
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_tab_btm()
{
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_post_tab()
{
}

void FCA_def_pre_infos()
{
	printf("  ");
}

void FCA_def_infos(const char format[], ...)
{
	va_list argptr;
	
	FCA_ansi_chs(35);
	va_start(argptr,format);
	vprintf(format, argptr);
	va_end(argptr);
	FCA_ansi_chs(0);
}

void FCA_def_num(int n, const char text[])
{
	FCA_ansi_chs(35);
	FCA_print_nb(n, text);
	FCA_ansi_chs(0);
}

void FCA_def_info_size(FFSS_LongField n, const char text[])
{
	FCA_ansi_chs(35);
	FCA_print_size(n, (char*)text);
	FCA_ansi_chs(0);
}

void FCA_def_main_num(int n, const char text[])
{
	FCA_ansi_chs(35);FCA_ansi_chs(1);
	FCA_print_nb(n, text);
	FCA_ansi_chs(0);
}

void FCA_def_post_infos()
{
	FCA_ansi_chs(0);printf("\n");
}

void FCA_def_size(FFSS_LongField n, const char text[])
{
	FCA_print_size(n, (char*)text);
}


void FCA_def_pre_err()
{
	FCA_ansi_chs_err(31);FCA_ansi_chs_err(1);
}

void FCA_def_post_err()
{
	FCA_ansi_chs_err(0);
}

void FCA_def_pre_warning()
{
	FCA_ansi_chs(33);
}

void FCA_def_post_warning()
{
	FCA_ansi_chs(0);
}

void FCA_def_pre_ok()
{
	FCA_ansi_chs(32);
}

void FCA_def_post_ok()
{
	FCA_ansi_chs(0);
}

void FCA_def_pre_serv(const char *domain, const char *name, long int state, bool isName)
{
	unsigned int col, style;
	
	if(state==FFSS_STATE_OFF) {
		col=33;
		style=33;
	} else if(state==FFSS_STATE_QUIET) {
		col=36;
		style=36;
	} else {
		col=37;
		style=37;
	}
	FCA_ansi_chs(col);
	FCA_ansi_chs(style);
}

void FCA_def_post_serv(bool isName)
{
	FCA_ansi_chs(0);
}
	
void FCA_def_pre_dir(const char *prefx, const char *name, bool isName)
{
	FCA_ansi_chs(33);	/* yellow */
}
void FCA_def_post_dir(bool isName)
{
	FCA_ansi_chs(0);
}

void FCA_def_pre_file(const char *prefx, const char *name, bool isName)
{
	FCA_ansi_chs(32);	/* green */
}

void FCA_def_post_file(bool isName)
{
	FCA_ansi_chs(0);
}
	
void FCA_def_pre_file_exec(const char *prefx, const char *name, bool isName)
{
	FCA_ansi_chs(32);	/* green */
}

bool FCA_def_pre_path(const char *domain, const char *path, long int state, bool isName, bool isSamba, bool isDir)
{
	unsigned int col, style;
	
	if(state & FFSS_STATE_OFF) {
		col=33;
		style=33;
	} else if(state & FFSS_STATE_QUIET) {
		col=36;
		style=36;
	} else {
		col=37;
		style=37;
	}
	if(isSamba) {
		col=35;
		style=35;
	}
	FCA_ansi_chs(col);
	FCA_ansi_chs(style);
	return false;
}

void FCA_def_post_path(bool isName)
{
	FCA_ansi_chs(0);
}

void FCA_def_post_file_exec(bool isName)
{
	printf("*");
	FCA_ansi_chs(0);
}

void FCA_def_progr_bar()
{
	int i, s;
	
		/* download progress bar */
	FCA_progr++;
	if(FCA_progr>50) {
		FCA_progr=0;
		if( FCA_VAR_IS_ON(FCA_can_ansi) ) {
				/* hardcoded size: 80 caracters */
			printf("\r");
			FCA_ansi_chs(32);FCA_ansi_chs(1);
			printf("[");
			FCA_ansi_chs(0);FCA_ansi_chs(44);
			s=64*FCA_dw_amount/FCA_dw_size;
			for(i=0; i<s; i++)
				printf(" ");
			FCA_ansi_chs(0);
			for(; i<64; i++)
				printf(" ");
			FCA_ansi_chs(32);FCA_ansi_chs(1);
			printf("]");
			FCA_ansi_chs(0);
			printf(" %d%% ", (int)(100*FCA_dw_amount/FCA_dw_size));
			FCA_print_size(12.3,"%4d");
			printf("B/s");
			FCA_ansi_clrl();
		} else
			printf(".");
			/* we must flush the buffer */
		fflush(stdout);
	}
}

void FCA_def_dw_ok(char *file, float rate)
{
	printf("\r ");
	FCA_print_size((int)rate,"%4d");
	printf("B/s  %s", file);
	FCA_ansi_clrl();
	printf("\n");
}
