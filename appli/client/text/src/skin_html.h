/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	default skin
 */

#ifndef _FCA_HTML_SKIN_
#define _FCA_HTML_SKIN_

#include "skin.h"

extern FCA_Tps FCA_html_ps;

void FCA_html_init();

	/* common to all html skins */
void FCA_dir_link(const char *dir);
void FCA_file_link(const char *file);
void FCA_dir_arg(const char *dir, bool isFile);
void FCA_pre_link();
void FCA_post_link(bool firstArg);
void FCA_form_hidden_args();
void FCA_my_url(bool isDownload);
void FCA_sep_link(char *path);

#endif	/* _FCA_HTML_SKIN_ */
