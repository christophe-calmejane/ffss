/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	skin functions
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

#include "client.h"
#include "skin.h"

#include <stdarg.h>


/*		TO CREATE A NEW SKIN :
	1) create a skin_<skin>.c and a skin_<skin>.h
	2) complete skin.h, "SKINS DEFINITION"
	3) fill the skins structure bellow
 */

	/* fill the skin structure here */
const FCA_Tskin FCA_SKINS[]={
/*
	 skin's name	pointer to the		pointer to the init	adapted to CGI
			 paint structure	 function
*/
	{"default",	&FCA_def_ps,		FCA_default_init,	false	},
	{"script",	&FCA_scr_ps,		FCA_script_init,	false	},
	{"html",	&FCA_html_ps,		FCA_html_init,		true	},
	{"html_fleming",&FCA_htmlfl_ps,		FCA_htmlfl_init,	true	},
	{"html_playlist",&FCA_playlist_ps,	FCA_playlist_init,	true	},
	{NULL,		NULL,			NULL,			false	}
};

int FCA_tab_width=5;


void FCA_load_skins()
{
	const FCA_Tskin *ps;

	ps=FCA_SKINS;
	while(ps && ps->name) {
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "Loading skin %s", ps->name);
		ps->init();
		ps++;
	}
}

void FCA_prog_begin()
{
	if(FCA_skin->ps->prog_begin)
		FCA_skin->ps->prog_begin();
}

void FCA_pre_listing(char *path)
{
	if(FCA_skin->ps->pre_listing)
		FCA_skin->ps->pre_listing(path);
}

void FCA_pre_search_ans(const char *query, const char *domain)
{
	if(FCA_skin->ps->pre_search_ans)
		FCA_skin->ps->pre_search_ans(query, domain);
}

void FCA_tab_top()
{
	if(FCA_skin->ps->tab_top)
		FCA_skin->ps->tab_top();
}

void FCA_tab_title(const char title[])
{
	if(FCA_skin->ps->tab_title)
		FCA_skin->ps->tab_title(title);
}

void FCA_tab_untitle()
{
	if(FCA_skin->ps->tab_untitle)
		FCA_skin->ps->tab_untitle();
}


void FCA_tab_pre_stitle()
{
	if(FCA_skin->ps->tab_pre_stitle)
		FCA_skin->ps->tab_pre_stitle();
}

void FCA_tab_stitle(const char name[], const unsigned int size)
{
	if(FCA_skin->ps->tab_stitle)
		FCA_skin->ps->tab_stitle(name, size);
}

void FCA_tab_int_stitle()
{
	if(FCA_skin->ps->tab_int_stitle)
		FCA_skin->ps->tab_int_stitle();
}

void FCA_tab_post_stitle()
{
	if(FCA_skin->ps->tab_post_stitle)
		FCA_skin->ps->tab_post_stitle();
}


void FCA_tab_pre_bar()
{
	if(FCA_skin->ps->tab_pre_bar)
		FCA_skin->ps->tab_pre_bar();
}

void FCA_tab_bar(const unsigned int size)
{
	if(FCA_skin->ps->tab_bar)
		FCA_skin->ps->tab_bar(size);
}

void FCA_tab_int_bar()
{
	if(FCA_skin->ps->tab_int_bar)
		FCA_skin->ps->tab_int_bar();
}

void FCA_tab_post_bar()
{
	if(FCA_skin->ps->tab_post_bar)
		FCA_skin->ps->tab_post_bar();
}


void FCA_tab_pre_item()
{
	if(FCA_skin->ps->tab_pre_item)
		FCA_skin->ps->tab_pre_item();
}

void FCA_pre_tab_item()
{
	if(FCA_skin->ps->pre_tab_item)
		FCA_skin->ps->pre_tab_item();
}

void FCA_tab_item(const char name[], const unsigned int size)
{
	if(FCA_skin->ps->tab_item)
		FCA_skin->ps->tab_item(name, size);
}

void FCA_post_tab_item()
{
	if(FCA_skin->ps->post_tab_item)
		FCA_skin->ps->post_tab_item();
}

void FCA_tab_int_item()
{
	if(FCA_skin->ps->tab_int_item)
		FCA_skin->ps->tab_int_item();
}

void FCA_tab_post_item()
{
	if(FCA_skin->ps->tab_post_item)
		FCA_skin->ps->tab_post_item();
}


void FCA_tab_btm()
{
	if(FCA_skin->ps->tab_btm)
		FCA_skin->ps->tab_btm();
}


void FCA_post_tab()
{
	if(FCA_skin->ps->post_tab)
		FCA_skin->ps->post_tab();
}


void FCA_pre_infos()
{
	if(FCA_skin->ps->pre_infos)
		FCA_skin->ps->pre_infos();
}

void FCA_infos(const char format[], ...)
{
	char Str[1024];
	va_list argptr;

	if(FCA_skin->ps->infos) {
		va_start(argptr,format);
		vsnprintf(Str, 1023, format, argptr);
		va_end(argptr);
		FCA_skin->ps->infos(Str);
	}
}

void FCA_num(int n, const char text[])
{
	if(FCA_skin->ps->num)
		FCA_skin->ps->num(n, text);
}

void FCA_info_size(FFSS_LongField n, const char text[])
{
	if(FCA_skin->ps->info_size)
		FCA_skin->ps->info_size(n, text);
}

void FCA_main_num(int n, const char text[])
{
	if(FCA_skin->ps->main_num)
		FCA_skin->ps->main_num(n, text);
}

void FCA_post_infos()
{
	if(FCA_skin->ps->post_infos)
		FCA_skin->ps->post_infos();
}

void FCA_size(FFSS_LongField n, const char text[])
{
	if(FCA_skin->ps->size)
		FCA_skin->ps->size(n, text);
}


void FCA_pre_err()
{
	if(FCA_skin->ps->pre_err)
		FCA_skin->ps->pre_err();
}

void FCA_post_err()
{
	if(FCA_skin->ps->post_err)
		FCA_skin->ps->post_err();
}


void FCA_pre_warning()
{
	if(FCA_skin->ps->pre_warning)
		FCA_skin->ps->pre_warning();
}

void FCA_post_warning()
{
	if(FCA_skin->ps->post_warning)
		FCA_skin->ps->post_warning();
}


void FCA_pre_ok()
{
	if(FCA_skin->ps->pre_ok)
		FCA_skin->ps->pre_ok();
}

void FCA_post_ok()
{
	if(FCA_skin->ps->post_ok)
		FCA_skin->ps->post_ok();
}


void FCA_pre_serv(const char *domain, const char *name, long int state, bool isName)
{
	if(FCA_skin->ps->pre_serv)
		FCA_skin->ps->pre_serv(domain, name,state, isName);
}

void FCA_post_serv(bool isName)
{
	if(FCA_skin->ps->post_serv)
		FCA_skin->ps->post_serv(isName);
}


void FCA_pre_dir(const char *prefx, const char *name, bool isName)
{
	if(FCA_skin->ps->pre_dir)
		FCA_skin->ps->pre_dir(prefx,name,isName);
}

void FCA_post_dir(bool isName)
{
	if(FCA_skin->ps->post_dir)
		FCA_skin->ps->post_dir(isName);
}


void FCA_pre_file(const char *prefx, const char *name, bool isName)
{
	if(FCA_skin->ps->pre_file)
		FCA_skin->ps->pre_file(prefx,name,isName);
}

void FCA_post_file(bool isName)
{
	if(FCA_skin->ps->post_file)
		FCA_skin->ps->post_file(isName);
}

void FCA_pre_file_exec(const char *prefx, const char *name, bool isName)
{
	if(FCA_skin->ps->pre_file_exec)
		FCA_skin->ps->pre_file_exec(prefx,name,isName);
}

void FCA_post_file_exec(bool isName)
{
	if(FCA_skin->ps->post_file_exec)
		FCA_skin->ps->post_file_exec(isName);
}

bool FCA_pre_path(const char *domain, const char *path, long int state, bool isName, bool isSamba, bool isDir)
{
	if(FCA_skin->ps->pre_path)
		return FCA_skin->ps->pre_path(domain, path, state,isName, isSamba, isDir);
	return false;
}

void FCA_post_path(bool isName)
{
	if(FCA_skin->ps->post_path)
		FCA_skin->ps->post_path(isName);
}

void FCA_post_listing(char *path)
{
	if(FCA_skin->ps->post_listing)
		FCA_skin->ps->post_listing(path);
}

void FCA_post_search_ans(const char *query)
{
	if(FCA_skin->ps->post_search_ans)
		FCA_skin->ps->post_search_ans(query);
}

void FCA_progr_bar()
{
	if(FCA_skin->ps->progr_bar)
		FCA_skin->ps->progr_bar();
}

void FCA_print_dw_ok(char *file, float rate)
{
	if(FCA_skin->ps->dw_ok)
		FCA_skin->ps->dw_ok(file,rate);
}

void FCA_prog_end()
{
	if(FCA_skin->ps->prog_end)
		FCA_skin->ps->prog_end();
}
