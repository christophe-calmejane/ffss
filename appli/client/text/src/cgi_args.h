/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	CGI functions
 */
/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef _FCA_CGI_H_
#define _FCA_CGI_H_

#include <ffss.h>

extern bool FCA_CGI_mode;

#ifdef CGI

extern char FCA_prefx[FFSS_MAX_PATH_LENGTH];
extern char FCA_html_page[FFSS_MAX_PATH_LENGTH];
extern char FCA_img_prefx[FFSS_MAX_PATH_LENGTH];
extern bool FCA_farg;	/* if we must use '?' or '&' to give arguments */
extern bool FCA_can_header;	/* if we can send HTTP headers */

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
void FCA_init_download_headers(char *filename);

#endif	/* CGI */

char *FCA_cgi_escape_special_chars(const char *str);
bool FCA_isInCGImode();

#endif	/* CGI_H */
