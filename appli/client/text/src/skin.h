/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	skin functions
 */

#ifndef _FCA_SKIN_H_
#define _FCA_SKIN_H_

#include "common.h"
#include <ffss.h>

typedef struct
{
	void (*prog_begin)();
	
	void (*pre_listing)(char *path);
	void (*pre_search_ans)(const char *query);
	
	void (*tab_top)();
	void (*tab_title)(const char title[]);
	void (*tab_untitle)();
	
	void (*tab_pre_stitle)();
	void (*tab_stitle)(const char name[], const unsigned int size);
	void (*tab_int_stitle)();
	void (*tab_post_stitle)();
	
	void (*tab_pre_bar)();
	void (*tab_bar)(const unsigned int size);
	void (*tab_int_bar)();
	void (*tab_post_bar)();
	
	void (*tab_pre_item)();
	void (*pre_tab_item)();
	void (*tab_item)(const char name[], unsigned int size);
	void (*post_tab_item)();
	void (*tab_int_item)();
	void (*tab_post_item)();
	
	void (*tab_btm)();
	
	void (*post_tab)();
	
	void (*pre_infos)();
	void (*infos)(const char format[], ...);
	void (*num)(int n, const char text[]);
	void (*info_size)(FFSS_LongField n, const char text[]);
	void (*main_num)(int n, const char text[]);
	void (*post_infos)();
	
	void (*size)(FFSS_LongField n, const char text[]);
	
	void (*pre_err)();
	void (*post_err)();
	
	void (*pre_warning)();
	void (*post_warning)();
	
	void (*pre_ok)();
	void (*post_ok)();

	void (*pre_serv)(const char *domain, const char *name, long int state, bool isName);
	void (*post_serv)(bool isName);
	
	void (*pre_dir)(const char *prefx, const char *name, bool isName);
	void (*post_dir)(bool isName);
	
	void (*pre_file)(const char *prefx, const char *name, bool isName);
	void (*post_file)(bool isName);
	
	void (*pre_file_exec)(const char *prefx, const char *name, bool isName);
	void (*post_file_exec)(bool isName);
	
	void (*post_listing)(char *path);
	void (*post_search_ans)(const char *query);
	
	void (*prog_end)();
} FCA_Tps, *FCA_Pps;	/* paint structure */

typedef struct
{
	char *name;
	FCA_Pps ps;	/* paint structure */
	void (*init)();
	bool canCGI;
} FCA_Tskin, *FCA_Pskin;

extern const FCA_Tskin FCA_SKINS[];
extern int FCA_tab_width;


	/* SKINS DEFINITIONS */

	/* 1) include your skin here */
#include "skin_default.h"
#include "skin_script.h"
#include "skin_html.h"
#include "skin_html_fleming.h"
#include "skin_html_playlist.h"

	/* 2) complete these defines */
#define FCA_SKINLIST	"default", "script", "html", "html_fleming", "html_playlist"

	/* 3) complete skin-specific envrironment variables (optional) */
	/*
		variable's	description			values		on_var_change
		name								 callback
	*/
#define FCA_SKIN_VARS	\
	{"html_prefix",		"to add before links",		"url",		NULL}, \
	{"html_img_prefix",	"to add before image locations","url",		NULL}, \
	{"html_firstarg",	"if in a link we can use '?', otherwise we use '&'","on/off",NULL}, \
	{"html_dw_prefix",	"to add before download links",	"url",		NULL}, \
	{"html_included_doc",	"if this is an included document (no <html>...)","on/off",NULL}, \
	{"script_delim",	"fields delimiter",		"character",	NULL}, \
	{"me",			"my name",			"name",		NULL},

	/* list of possible values to variables */
#define FCA_SKIN_VAR_VALUES \
	{"", "ffss-client", NULL}, \
	{"", "http://localhost/ffss/img/", NULL}, \
	{"on","off",NULL}, \
	{"", "ffss-client", NULL}, \
	{"on","off",NULL}, \
	{":", ",", "|", "/", "&", NULL}, \
	{"", FCA_NAME, NULL},
	/* default values */
#define FCA_SKIN_ENV_VALUES \
	"ffss-client", \
	"", \
	"on", \
	"ffss-client", \
	"off", \
	":", \
	FCA_NAME

	/* defines for environment variables */
#define FCA_html_prefix		FCA_env[FCA_skin_env_index  ]
#define FCA_html_img_prefix	FCA_env[FCA_skin_env_index+1]
#define FCA_html_firstarg	FCA_env[FCA_skin_env_index+2]
#define FCA_html_dw_prefix	FCA_env[FCA_skin_env_index+3]
#define FCA_html_included_doc	FCA_env[FCA_skin_env_index+4]
#define FCA_script_delim	FCA_env[FCA_skin_env_index+5][0]


void FCA_load_skins();

void FCA_prog_begin();

void FCA_pre_listing(char *path);
void FCA_pre_search_ans(const char *query);

void FCA_tab_top();
void FCA_tab_title(const char title[]);
void FCA_tab_untitle();
	
void FCA_tab_pre_stitle();
void FCA_tab_stitle(const char name[], const unsigned int size);
void FCA_tab_int_stitle();
void FCA_tab_post_stitle();
	
void FCA_tab_pre_bar();
void FCA_tab_bar(const unsigned int size);
void FCA_tab_int_bar();
void FCA_tab_post_bar();
	
void FCA_tab_pre_item();
void FCA_pre_tab_item();
void FCA_tab_item(const char name[], const unsigned int size);
void FCA_post_tab_item();
void FCA_tab_int_item();
void FCA_tab_post_item();
	
void FCA_tab_btm();
	
void FCA_post_tab();
	
void FCA_pre_infos();
void FCA_infos(const char format[], ...);
void FCA_num(int n, const char text[]);
void FCA_info_size(const FFSS_LongField n, const char text[]);
void FCA_main_num(int n, const char text[]);
void FCA_post_infos();

void FCA_size(FFSS_LongField n, const char text[]);

void FCA_pre_err();
void FCA_post_err();
	
void FCA_pre_warning();
void FCA_post_warning();

void FCA_pre_ok();
void FCA_post_ok();

void FCA_pre_serv(const char *domain, const char *name, long int state, bool isName);
void FCA_post_serv(bool isName);

void FCA_pre_dir(const char *prefx, const char *name, bool isName);
void FCA_post_dir(bool isName);
	
void FCA_pre_file(const char *prefx, const char *name, bool isName);
void FCA_post_file(bool isName);
	
void FCA_pre_file_exec(const char *prefx, const char *name, bool isName);
void FCA_post_file_exec(bool isName);

void FCA_post_listing(char *path);
void FCA_post_search_ans(const char *query);

void FCA_prog_end();

#endif /* ifndef _FCA_SKIN_H_ */
