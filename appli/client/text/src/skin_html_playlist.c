/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	html skin for playlists via http
 */

#include <stdio.h>
#include <stdarg.h>

#include "display.h"
#include "skin.h"
#include "datastruct.h"
#include "client.h"
#include "skin_html_playlist.h"
#include "cgi_args.h"

void FCA_playlist_prog_begin();

void FCA_playlist_pre_listing(char *path);
void FCA_playlist_pre_search_ans(const char *query);

void FCA_playlist_tab_top();
void FCA_playlist_tab_title(const char title[]);
void FCA_playlist_tab_untitle();

void FCA_playlist_tab_pre_stitle();
void FCA_playlist_tab_stitle(const char name[], const unsigned int size);
void FCA_playlist_tab_post_stitle();

void FCA_playlist_tab_pre_item();
void FCA_playlist_pre_tab_item();
void FCA_playlist_tab_item(const char name[], const unsigned int size);
void FCA_playlist_post_tab_item();
void FCA_playlist_tab_post_item();

void FCA_playlist_tab_btm();

void FCA_playlist_pre_infos();
void FCA_playlist_infos(const char format[], ...);
void FCA_playlist_num(int n, const char text[]);
void FCA_playlist_info_size(FFSS_LongField n, const char text[]);
void FCA_playlist_main_num(int n, const char text[]);
void FCA_playlist_post_infos();

void FCA_playlist_size(FFSS_LongField n, const char text[]);

void FCA_playlist_pre_err();
void FCA_playlist_post_err();

void FCA_playlist_pre_warning();
void FCA_playlist_post_warning();

void FCA_playlist_pre_ok();
void FCA_playlist_post_ok();

void FCA_playlist_pre_serv(const char *domain, const char *name, long int state, bool isName);
void FCA_playlist_post_serv(bool isName);

void FCA_playlist_pre_dir(const char *prefx, const char *name, bool isName);
void FCA_playlist_post_dir(bool isName);

void FCA_playlist_pre_file(const char *prefx, const char *name, bool isName);
void FCA_playlist_post_file(bool isName);

void FCA_playlist_pre_file_exec(const char *prefx, const char *name, bool isName);
void FCA_playlist_post_file_exec(bool isName);

void FCA_playlist_post_listing(char *path);
void FCA_playlist_post_search_ans(const char *query);

void FCA_playlist_prog_end();


FCA_Tps FCA_playlist_ps;


void FCA_playlist_init()
{
	FCA_playlist_ps.prog_begin=FCA_playlist_prog_begin;
	
	FCA_playlist_ps.pre_listing=FCA_playlist_pre_listing;
	FCA_playlist_ps.pre_search_ans=FCA_playlist_pre_search_ans;
	
	FCA_playlist_ps.tab_top=FCA_playlist_tab_top;
	FCA_playlist_ps.tab_title=FCA_playlist_tab_title;
	FCA_playlist_ps.tab_untitle=FCA_playlist_tab_untitle;
	
	FCA_playlist_ps.tab_pre_stitle=FCA_playlist_tab_pre_stitle;
	FCA_playlist_ps.tab_stitle=FCA_playlist_tab_stitle;
	FCA_playlist_ps.tab_post_stitle=FCA_playlist_tab_post_stitle;
	
	FCA_playlist_ps.tab_pre_item=FCA_playlist_tab_pre_item;
	FCA_playlist_ps.pre_tab_item=FCA_playlist_pre_tab_item;
	FCA_playlist_ps.tab_item=FCA_playlist_tab_item;
	FCA_playlist_ps.post_tab_item=FCA_playlist_post_tab_item;
	FCA_playlist_ps.tab_post_item=FCA_playlist_tab_post_item;
	
	FCA_playlist_ps.tab_btm=FCA_playlist_tab_btm;

	FCA_playlist_ps.pre_infos=FCA_playlist_pre_infos;
	FCA_playlist_ps.infos=FCA_playlist_infos;
	FCA_playlist_ps.num=FCA_playlist_num;
	FCA_playlist_ps.info_size=FCA_playlist_info_size;
	FCA_playlist_ps.main_num=FCA_playlist_main_num;
	FCA_playlist_ps.post_infos=FCA_playlist_post_infos;
	
	FCA_playlist_ps.size=FCA_playlist_size;
	
	FCA_playlist_ps.pre_err=FCA_playlist_pre_err;
	FCA_playlist_ps.post_err=FCA_playlist_post_err;

	FCA_playlist_ps.pre_warning=FCA_playlist_pre_warning;
	FCA_playlist_ps.post_warning=FCA_playlist_post_warning;

	FCA_playlist_ps.pre_ok=FCA_playlist_pre_ok;
	FCA_playlist_ps.post_ok=FCA_playlist_post_ok;

	FCA_playlist_ps.pre_serv=FCA_playlist_pre_serv;
	FCA_playlist_ps.post_serv=FCA_playlist_post_serv;

	FCA_playlist_ps.pre_dir=FCA_playlist_pre_dir;
	FCA_playlist_ps.post_dir=FCA_playlist_post_dir;

	FCA_playlist_ps.pre_file=FCA_playlist_pre_file;
	FCA_playlist_ps.post_file=FCA_playlist_post_file;

	FCA_playlist_ps.pre_file_exec=FCA_playlist_pre_file_exec;
	FCA_playlist_ps.post_file_exec=FCA_playlist_post_file_exec;

	FCA_playlist_ps.post_listing=FCA_playlist_post_listing;
	FCA_playlist_ps.post_search_ans=FCA_playlist_post_search_ans;
	
	FCA_playlist_ps.prog_end=FCA_playlist_prog_end;
}


void FCA_playlist_prog_begin()
{}
void FCA_playlist_pre_listing(char *path)
{}
void FCA_playlist_pre_search_ans(const char *query)
{}
void FCA_playlist_tab_top()
{}
void FCA_playlist_tab_title(const char title[])
{}
void FCA_playlist_tab_untitle()
{}
void FCA_playlist_tab_pre_stitle()
{}
void FCA_playlist_tab_stitle(const char name[], const unsigned int size)
{}
void FCA_playlist_tab_post_stitle()
{}
void FCA_playlist_tab_pre_item()
{}
void FCA_playlist_pre_tab_item()
{}
void FCA_playlist_tab_item(const char name[], const unsigned int size)
{}
void FCA_playlist_post_tab_item()
{}
void FCA_playlist_tab_post_item()
{}
void FCA_playlist_tab_btm()
{}
void FCA_playlist_pre_infos()
{}
void FCA_playlist_infos(const char format[], ...)
{}
void FCA_playlist_num(int n, const char text[])
{}
void FCA_playlist_info_size(FFSS_LongField n, const char text[])
{}
void FCA_playlist_main_num(int n, const char text[])
{}
void FCA_playlist_post_infos()
{}
void FCA_playlist_size(FFSS_LongField n, const char text[])
{}
void FCA_playlist_pre_err()
{}
void FCA_playlist_post_err()
{}
void FCA_playlist_pre_warning()
{}
void FCA_playlist_post_warning()
{}
void FCA_playlist_pre_ok()
{}
void FCA_playlist_post_ok()
{}
void FCA_playlist_pre_serv(const char *domain, const char *name, long int state, bool isName)
{}
void FCA_playlist_post_serv(bool isName)
{}

void FCA_playlist_pre_dir(const char *prefx, const char *name, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];
	
	if(isName) {
		if(prefx[0]=='\0')
			FCA_playlist_dir_link(name);
		else {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "%s/%s", prefx, name);
			FCA_playlist_dir_link(all);
		}
	}
}

void FCA_playlist_post_dir(bool isName)
{}

void FCA_playlist_pre_file(const char *prefx, const char *name, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];
	
	if(isName) {
		if(prefx[0]=='\0')
			FCA_playlist_file_link(name);
		else {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "%s/%s", prefx, name);
			FCA_playlist_file_link(all);
		}
	}
}

void FCA_playlist_post_file(bool isName)
{}

void FCA_playlist_pre_file_exec(const char *prefx, const char *name, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];
	
	if(isName) {
		if(prefx[0]=='\0')
			FCA_playlist_file_link(name);
		else {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "%s/%s", prefx, name);
			FCA_playlist_file_link(all);
		}
	}
}

void FCA_playlist_post_file_exec(bool isName)
{}
void FCA_playlist_post_listing(char *path)
{}
void FCA_playlist_post_search_ans(const char *query)
{}
void FCA_playlist_prog_end()
{}

void FCA_playlist_dir_link(const char *dir)
{
	FCA_playlist_pre_link();
	FCA_playlist_dir_arg(dir, false);
	FCA_playlist_post_link(false);
}

void FCA_playlist_file_link(const char *file)
{
#ifdef CGI_DOWNLOADS
	FCA_playlist_pre_link();
	FCA_playlist_dir_arg(file, true);
	printf("&download=1");
	FCA_playlist_post_link(false);
#endif
}

void FCA_playlist_dir_arg(const char *dir, bool isFile)
{
	char *tl;
	
	tl=FCA_cgi_escape_special_chars(dir);
	FCA_playlist_my_url(isFile);
	printf("%cdir=%s",
		FCA_VAR_IS_ON(FCA_html_firstarg)?'?':'&',
		tl);
	free(tl);
}

void FCA_playlist_pre_link()
{}

void FCA_playlist_post_link(bool firstArg)
{
	char *p;
	
		/* firstArg: if there was an argument before */
	printf("%c", (!FCA_VAR_IS_ON(FCA_html_firstarg) || !firstArg)?'&':'?');
	printf("prefix=%s",
		p=FCA_cgi_escape_special_chars(FCA_html_prefix) );
	free(p);
	printf("&img_prefix=%s",
		p=FCA_cgi_escape_special_chars(FCA_html_prefix) );
	free(p);
	printf("&firstarg=%s",
		p=FCA_cgi_escape_special_chars(FCA_html_firstarg) );
	free(p);
	printf("&skin=%s",
		p=FCA_cgi_escape_special_chars(FCA_skin_name) );
	free(p);
	printf("&master=%s",
		p=FCA_cgi_escape_special_chars(FCA_master) );
	free(p);
	printf("&debug=%s",
		p=FCA_cgi_escape_special_chars(FCA_debuglevel) );
	printf("&included_doc=%s", FCA_html_included_doc);
	free(p);
	printf("\n");
}

void FCA_playlist_my_url(bool isDownload)
{
	if(isDownload)
		printf("%s",
			FCA_html_dw_prefix
		);
	else
		printf("%sblablabla",
			FCA_html_prefix
		);
}
