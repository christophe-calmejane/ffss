/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	html skin for playlists via http
 */

#ifndef _FCA_HTML_PLAYLIST_SKIN_
#define _FCA_HTML_PLAYLIST_SKIN_

#include "skin.h"

extern FCA_Tps FCA_playlist_ps;

void FCA_playlist_init();
void FCA_playlist_dir_link(const char *dir);
void FCA_playlist_file_link(const char *file);
void FCA_playlist_dir_arg(const char *dir, bool isFile);
void FCA_playlist_pre_link();
void FCA_playlist_post_link(bool firstArg);
void FCA_playlist_my_url(bool isDownload);

#endif	/* _FCA_HTML_PLAYLIST_SKIN_ */
