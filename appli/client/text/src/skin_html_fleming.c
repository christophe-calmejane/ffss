/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	html skin for fleming website
 */

#include <stdio.h>
#include <stdarg.h>

#include "display.h"
#include "skin.h"
#include "datastruct.h"
#include "client.h"
#include "skin_html_fleming.h"
#include "cgi_args.h"

void FCA_htmlfl_prog_begin();

void FCA_htmlfl_pre_listing(char *path);
void FCA_htmlfl_pre_search_ans(const char *query);

void FCA_htmlfl_tab_top();
void FCA_htmlfl_tab_title(const char title[]);
void FCA_htmlfl_tab_untitle();

void FCA_htmlfl_tab_pre_stitle();
void FCA_htmlfl_tab_stitle(const char name[], const unsigned int size);
void FCA_htmlfl_tab_post_stitle();

void FCA_htmlfl_tab_pre_item();
void FCA_htmlfl_pre_tab_item();
void FCA_htmlfl_tab_item(const char name[], const unsigned int size);
void FCA_htmlfl_post_tab_item();
void FCA_htmlfl_tab_post_item();

void FCA_htmlfl_tab_btm();

void FCA_htmlfl_pre_infos();
void FCA_htmlfl_infos(const char format[], ...);
void FCA_htmlfl_num(int n, const char text[]);
void FCA_htmlfl_info_size(FFSS_LongField n, const char text[]);
void FCA_htmlfl_main_num(int n, const char text[]);
void FCA_htmlfl_post_infos();

void FCA_htmlfl_size(FFSS_LongField n, const char text[]);

void FCA_htmlfl_pre_err();
void FCA_htmlfl_post_err();

void FCA_htmlfl_pre_warning();
void FCA_htmlfl_post_warning();

void FCA_htmlfl_pre_ok();
void FCA_htmlfl_post_ok();

void FCA_htmlfl_pre_serv(const char *domain, const char *name, long int state, bool isName);
void FCA_htmlfl_post_serv(bool isName);

void FCA_htmlfl_pre_dir(const char *prefx, const char *name, bool isName);
void FCA_htmlfl_post_dir(bool isName);

void FCA_htmlfl_pre_file(const char *prefx, const char *name, bool isName);
void FCA_htmlfl_post_file(bool isName);

void FCA_htmlfl_pre_file_exec(const char *prefx, const char *name, bool isName);
void FCA_htmlfl_post_file_exec(bool isName);

bool FCA_htmlfl_pre_path(const char *domain, const char *path, long int state, bool isName, bool isSamba, bool isDir);
void FCA_htmlfl_post_path(bool isName);

void FCA_htmlfl_post_listing(char *path);
void FCA_htmlfl_post_search_ans(const char *query);

void FCA_htmlfl_prog_end();

void FCA_htmlfl_form_hidden_args(bool canskin);


FCA_Tps FCA_htmlfl_ps;


void FCA_htmlfl_init()
{
	FCA_htmlfl_ps.prog_begin=FCA_htmlfl_prog_begin;
	
	FCA_htmlfl_ps.pre_listing=FCA_htmlfl_pre_listing;
	FCA_htmlfl_ps.pre_search_ans=FCA_htmlfl_pre_search_ans;
	
	FCA_htmlfl_ps.tab_top=FCA_htmlfl_tab_top;
	FCA_htmlfl_ps.tab_title=FCA_htmlfl_tab_title;
	FCA_htmlfl_ps.tab_untitle=FCA_htmlfl_tab_untitle;
	
	FCA_htmlfl_ps.tab_pre_stitle=FCA_htmlfl_tab_pre_stitle;
	FCA_htmlfl_ps.tab_stitle=FCA_htmlfl_tab_stitle;
	FCA_htmlfl_ps.tab_post_stitle=FCA_htmlfl_tab_post_stitle;
	
	FCA_htmlfl_ps.tab_pre_item=FCA_htmlfl_tab_pre_item;
	FCA_htmlfl_ps.pre_tab_item=FCA_htmlfl_pre_tab_item;
	FCA_htmlfl_ps.tab_item=FCA_htmlfl_tab_item;
	FCA_htmlfl_ps.post_tab_item=FCA_htmlfl_post_tab_item;
	FCA_htmlfl_ps.tab_post_item=FCA_htmlfl_tab_post_item;
	
	FCA_htmlfl_ps.tab_btm=FCA_htmlfl_tab_btm;

	FCA_htmlfl_ps.pre_infos=FCA_htmlfl_pre_infos;
	FCA_htmlfl_ps.infos=FCA_htmlfl_infos;
	FCA_htmlfl_ps.num=FCA_htmlfl_num;
	FCA_htmlfl_ps.info_size=FCA_htmlfl_info_size;
	FCA_htmlfl_ps.main_num=FCA_htmlfl_main_num;
	FCA_htmlfl_ps.post_infos=FCA_htmlfl_post_infos;
	
	FCA_htmlfl_ps.size=FCA_htmlfl_size;
	
	FCA_htmlfl_ps.pre_err=FCA_htmlfl_pre_err;
	FCA_htmlfl_ps.post_err=FCA_htmlfl_post_err;

	FCA_htmlfl_ps.pre_warning=FCA_htmlfl_pre_warning;
	FCA_htmlfl_ps.post_warning=FCA_htmlfl_post_warning;

	FCA_htmlfl_ps.pre_ok=FCA_htmlfl_pre_ok;
	FCA_htmlfl_ps.post_ok=FCA_htmlfl_post_ok;

	FCA_htmlfl_ps.pre_serv=FCA_htmlfl_pre_serv;
	FCA_htmlfl_ps.post_serv=FCA_htmlfl_post_serv;

	FCA_htmlfl_ps.pre_dir=FCA_htmlfl_pre_dir;
	FCA_htmlfl_ps.post_dir=FCA_htmlfl_post_dir;

	FCA_htmlfl_ps.pre_file=FCA_htmlfl_pre_file;
	FCA_htmlfl_ps.post_file=FCA_htmlfl_post_file;

	FCA_htmlfl_ps.pre_file_exec=FCA_htmlfl_pre_file_exec;
	FCA_htmlfl_ps.post_file_exec=FCA_htmlfl_post_file_exec;
	
	FCA_htmlfl_ps.pre_path=FCA_htmlfl_pre_path;
	FCA_htmlfl_ps.post_path=FCA_htmlfl_post_path;

	FCA_htmlfl_ps.post_listing=FCA_htmlfl_post_listing;
	FCA_htmlfl_ps.post_search_ans=FCA_htmlfl_post_search_ans;
	
	FCA_htmlfl_ps.prog_end=FCA_htmlfl_prog_end;
}


void FCA_htmlfl_prog_begin()
{
if(!FCA_VAR_IS_ON(FCA_html_included_doc))
printf("<html>
<head>
 <title>ffss client</title>
</head>

<body>
 ");
printf("
<form name='browsing' action='");FCA_my_url(false);printf("' method=GET class='small'>");
printf("<center>\n");
printf(" <a href='javascript:history.back()'>&lt;-</a>&nbsp;\n");
printf(" ");FCA_dir_link("/$");printf("home</a>&nbsp;\n");
printf(" <a href='javascript:history.forward()'>-&gt;</a>\n");
printf("</center>\n");
FCA_htmlfl_form_hidden_args(true);
}

void FCA_htmlfl_pre_listing(char *path)
{
	char *p;
	const FCA_Tskin *ps;
	
	p=strrchr(path, '/');
	if(p && p!=path) {
		*p='\0';
		FCA_dir_link(path);printf("parent</a>\n");
		*p='/';
	}
printf("address: <input type='text' name='dir' value='%s' tabindex=1>
&nbsp;<input type='submit' value='go'><br>\n", path);printf("
<input type='hidden' name='s' value=''>
<input type='hidden' name='sdom' value=''>
</form>
<form name='search' action='");FCA_my_url(false);printf("' method=GET class='small'>");
FCA_htmlfl_form_hidden_args(false);printf("
<input type='hidden' name='dir' value=''>
<table border=0 width='100%%'>
 <tr>
  <td>search: <input type='text' name='s' value='' tabindex=2> on
   <select name='sdom'>
    <option value='all'>all domains
");
	p=FCA_get_a_domain();
	while(p) {
		if(SU_strcasecmp(p, "None"))
			printf("    <option>%s\n", p);
		p=FCA_get_a_domain();
	}
printf("   </select>&nbsp;<input type='submit' value='search'>
  </td><td>
   skin: 
   <select name='skin'>
");
	for(ps=FCA_SKINS; ps && ps->name; ps++) {
		if(ps->canCGI) {
			printf("    <option");
			if(!strcmp(ps->name, FCA_skin_name))
				printf(" selected");
			printf(">%s\n", ps->name);
		}
	}
printf("   </select>&nbsp;<input type='submit' value='ok'>
  </td>
 </tr>
</table>
<center>
 <h2>
  ");FCA_sep_link(path, "", true);printf("
 </h2>
</center>
</form>
");
}

void FCA_htmlfl_pre_search_ans(const char *query)
{
	char *p;
	const FCA_Tskin *ps;

printf("<font class='small'>address: </font><input type='text' name='dir' value='/$' tabindex=1>
&nbsp;<input type='submit' value='go'><br>\n");printf("
<input type='hidden' name='s' value=''>
<input type='hidden' name='sdom' value=''>
</form>
<form name='search' action='");FCA_my_url(false);printf("' method=GET class='small'>");
FCA_htmlfl_form_hidden_args(true);printf("
<input type='hidden' name='dir' value=''>
<table border=0 width='100%%'>
 <tr>
  <td>search: <input type='text' name='s' value='%s' tabindex=2> on", query);printf("
   <select name='sdom'>
    <option value='all'>all domains
");
	p=FCA_get_a_domain();
	while(p) {
		if(SU_strcasecmp(p, "None"))
			printf("    <option>%s\n", p);
		p=FCA_get_a_domain();
	}
printf("   </select>&nbsp;<input type='submit' value='search'>
  </td><td>
   skin: 
   <select name='skin'>
");
	for(ps=FCA_SKINS; ps && ps->name; ps++) {
		if(ps->canCGI)
			printf("    <option>%s\n", ps->name);
	}
printf("   </select>
  </td>
 </tr>
</table>
<center>
 <h2>
  search results for '%s'", query);printf("
 </h2>
</center>
</form>
");
}

void FCA_htmlfl_tab_top()
{
	printf("<center>\n<table width='100%%' border=1 class='small'>\n");
}

void FCA_htmlfl_tab_title(const char title[])
{

	printf(" <tr><td>\n");
	printf("  <h2>%s</h2>\n", title);
	printf(" </td></tr>\n");
}

void FCA_htmlfl_tab_untitle()
{
	printf("</table>\n<table width='100%%' border=1 class='small'>\n");
}

void FCA_htmlfl_tab_pre_stitle()
{
	printf(" <tr>\n");
}

void FCA_htmlfl_tab_stitle(const char name[], const unsigned int size)
{
	printf("  <td><b>%s</b></td>\n", name);
}

void FCA_htmlfl_tab_post_stitle()
{
	printf(" </tr>\n");
}

void FCA_htmlfl_tab_pre_item()
{
	printf(" <tr>\n");
}

void FCA_htmlfl_pre_tab_item()
{
	printf("  <td>");
}

void FCA_htmlfl_tab_item(const char name[], const unsigned int size)
{
	printf("%s", name);
}

void FCA_htmlfl_post_tab_item()
{
	printf("</td>\n");
}

void FCA_htmlfl_tab_post_item()
{
	printf(" </tr>\n");
}

void FCA_htmlfl_tab_btm()
{
	printf("</table>\n</center><br>\n");
}

void FCA_htmlfl_pre_infos()
{
	printf("<p class='small'><i>");
}

void FCA_htmlfl_infos(const char format[], ...)
{
	va_list argptr;

	va_start(argptr,format);
	vprintf(format, argptr);
	va_end(argptr);
}

void FCA_htmlfl_num(int n, const char text[])
{
	FCA_print_nb(n, text);
}

void FCA_htmlfl_info_size(FFSS_LongField n, const char text[])
{
	FCA_print_size(n, (char*)text);
}

void FCA_htmlfl_main_num(int n, const char text[])
{
	printf("<b>");
	FCA_print_nb(n, text);
	printf("</b>");
}

void FCA_htmlfl_post_infos()
{
	printf("</i></p><br>\n</center>\n");
}

void FCA_htmlfl_size(FFSS_LongField n, const char text[])
{
	FCA_print_size(n, (char*)text);
}

void FCA_htmlfl_pre_err()
{
	fprintf(FCA_err_stream, "<font color=red class='small'><b>");
}

void FCA_htmlfl_post_err()
{
	fprintf(FCA_err_stream, "</b></font><br>\n");
}

void FCA_htmlfl_pre_warning()
{
	printf("<font color=orange class='small'><b>");
}

void FCA_htmlfl_post_warning()
{
	printf("</b></font><br>\n");
}


void FCA_htmlfl_pre_ok()
{
	printf("<font color=green class='small'><b>");
}

void FCA_htmlfl_post_ok()
{
	printf("</b></font><br>\n");
}

void FCA_htmlfl_pre_serv(const char *domain, const char *name, long int state, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];

	if(state==FFSS_STATE_OFF)
		printf("<font color=gray class='small'>");
	else if(state==FFSS_STATE_QUIET)
		printf("<font color=brown class='small'>");
	else
		printf("<font color=black class='small'>");
	
	if(isName) {
		if(state!=FFSS_STATE_OFF) {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "/$/%s/%s", domain, name);
			FCA_dir_link(all);
		}
	}
}

void FCA_htmlfl_post_serv(bool isName)
{
	printf("</font>");
	if(isName)
		printf("</a>");
}

void FCA_htmlfl_pre_dir(const char *prefx, const char *name, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];
	
	if(isName) {
		if(prefx[0]=='\0')
			FCA_dir_link(name);
		else {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "%s/%s", prefx, name);
			FCA_dir_link(all);
		}
	}
}

void FCA_htmlfl_post_dir(bool isName)
{
	if(isName)
		printf("</a>/");
}

void FCA_htmlfl_pre_file(const char *prefx, const char *name, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];
	
	if(isName) {
		if(prefx[0]=='\0')
			FCA_file_link(name);
		else {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "%s/%s", prefx, name);
			FCA_file_link(all);
		}
	}
}

void FCA_htmlfl_post_file(bool isName)
{
	if(isName)
		printf("</a>");
}

void FCA_htmlfl_pre_file_exec(const char *prefx, const char *name, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];
	
	if(isName) {
		if(prefx[0]=='\0')
			FCA_file_link(name);
		else {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "%s/%s", prefx, name);
			FCA_file_link(all);
		}
	}
}

void FCA_htmlfl_post_file_exec(bool isName)
{
	if(isName)
		printf("</a>*");
}

bool FCA_htmlfl_pre_path(const char *domain, const char *path, long int state, bool isName, bool isSamba, bool isDir)
{
	char *all;
	char dom[FFSS_MAX_FILEPATH_LENGTH];
	
	if(state==FFSS_STATE_OFF)
		printf("<font color=gray>");
	else if(state==FFSS_STATE_QUIET)
		printf("<font color=brown>");
	else
		printf("<font color=black>");
	
	if(isName) {
		if(state!=FFSS_STATE_OFF) {
			all=strdup(path);
			snprintf(dom, FFSS_MAX_FILEPATH_LENGTH-1, "/$/%s/", domain);
			if(isSamba)
				FCA_smb_sep_link(all);
			else
				FCA_sep_link(all, dom, isDir);
			if(all)
				free(all);
		}
	}
	return true;
}

void FCA_htmlfl_post_path(bool isName)
{
	printf("</font>");
	if(isName)
		printf("</a>");
}


void FCA_htmlfl_post_listing(char *path)
{

}

void FCA_htmlfl_post_search_ans(const char *query)
{

}

void FCA_htmlfl_prog_end()
{
printf("<center><font  class='small'>\n");
printf(" <a href='javascript:history.back()'>&lt;-</a>&nbsp;\n");
printf(" ");FCA_dir_link("/$");printf("home</a>&nbsp;\n");
printf(" <a href='javascript:history.forward()'>-&gt;</a>\n");
printf("</font></center>
</form>
");
if(!FCA_VAR_IS_ON(FCA_html_included_doc))
printf("</body>
</html>
");
}

void FCA_htmlfl_form_hidden_args(bool canskin)
{
	char *p;
	
#ifdef CGI
	printf(" <input type='hidden' name='page' value='%s'>\n",
		FCA_html_page);
#endif
	printf(" <input type='hidden' name='prefix' value='%s'>\n",
		p=FCA_cgi_escape_special_chars(FCA_html_prefix) );
	free(p);
	printf(" <input type='hidden' name='img_prefix' value='%s'>\n",
		p=FCA_cgi_escape_special_chars(FCA_html_prefix) );
	free(p);
	printf(" <input type='hidden' name='firstarg' value='%s'>\n",
		p=FCA_cgi_escape_special_chars(FCA_html_firstarg) );
	free(p);
	if(canskin) {
		printf(" <input type='hidden' name='skin' value='%s'>\n",
			p=FCA_cgi_escape_special_chars(FCA_skin_name) );
		free(p);
	}
	printf(" <input type='hidden' name='master' value='%s'>\n",
		p=FCA_cgi_escape_special_chars(FCA_master) );
	free(p);
	printf(" <input type='hidden' name='debug' value='%s'>\n",
		p=FCA_cgi_escape_special_chars(FCA_debuglevel) );
	free(p);
}
