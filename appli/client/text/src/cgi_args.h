/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	CGI functions
 */



#ifndef _FCA_CGI_H_
#define _FCA_CGI_H_

#include <ffss.h>

extern bool FCA_CGI_mode;
extern char FCA_prefx[FFSS_MAX_PATH_LENGTH];
extern char FCA_img_prefx[FFSS_MAX_PATH_LENGTH];
extern bool FCA_farg;	/* if we must use '?' or '&' to give arguments */
extern bool FCA_can_header;	/* if we can send HTTP headers */

#ifdef CGI

typedef struct {
	const char *name;
	char *var;
	unsigned int max;
	int env_index;
} FCA_TCGI_arg;

typedef struct {
	const char *name;
	bool *var;
} FCA_TCGI_bool_arg;

	/* CGI-specific variables */
extern bool FCA_must_download;
extern char FCA_dir_to_list[FFSS_MAX_PATH_LENGTH];
extern char FCA_search[FFSS_MAX_KEYWORDS_LENGTH];
extern char FCA_search_dom[FFSS_MAX_DOMAIN_LENGTH];
extern const FCA_TCGI_arg FCA_CGI_ARGS[];
extern const FCA_TCGI_bool_arg FCA_CGI_BOOL_ARGS[];

	/* CGI-specific functions */
void FCA_read_cgi_args();

	/* alternatives to libcgi */
void FCA_init_headers();
void FCA_init_download_headers();

#endif	/* CGI */

char *FCA_cgi_escape_special_chars(const char *str);
bool FCA_isInCGImode();

#endif	/* CGI_H */
