/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	html skin for playlists via http
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
