/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	default skin
 */

#include <stdio.h>
#include <stdarg.h>

#include "display.h"
#include "skin.h"
#include "datastruct.h"
#include "client.h"
#include "skin_html.h"
#include "cgi_args.h"

void FCA_html_prog_begin();

void FCA_html_pre_listing(char *path);
void FCA_html_pre_search_ans(const char *query);

void FCA_html_tab_top();
void FCA_html_tab_title(const char title[]);
void FCA_html_tab_untitle();

void FCA_html_tab_pre_stitle();
void FCA_html_tab_stitle(const char name[], const unsigned int size);
void FCA_html_tab_post_stitle();

void FCA_html_tab_pre_item();
void FCA_html_pre_tab_item();
void FCA_html_tab_item(const char name[], const unsigned int size);
void FCA_html_post_tab_item();
void FCA_html_tab_post_item();

void FCA_html_tab_btm();

void FCA_html_infos(const char format[], ...);
void FCA_html_num(int n, const char text[]);
void FCA_html_info_size(FFSS_LongField n, const char text[]);
void FCA_html_main_num(int n, const char text[]);
void FCA_html_post_infos();

void FCA_html_size(FFSS_LongField n, const char text[]);

void FCA_html_pre_err();
void FCA_html_post_err();

void FCA_html_pre_warning();
void FCA_html_post_warning();

void FCA_html_pre_ok();
void FCA_html_post_ok();

void FCA_html_pre_serv(const char *domain, const char *name, long int state, bool isName);
void FCA_html_post_serv(bool isName);

void FCA_html_pre_dir(const char *prefx, const char *name, bool isName);
void FCA_html_post_dir(bool isName);

void FCA_html_pre_file(const char *prefx, const char *name, bool isName);
void FCA_html_post_file(bool isName);

void FCA_html_pre_file_exec(const char *prefx, const char *name, bool isName);
void FCA_html_post_file_exec(bool isName);

bool FCA_html_pre_path(const char *domain, const char *path, long int state, bool isName, bool isSamba, bool isDir);
void FCA_html_post_path(bool isName);

void FCA_html_post_listing(char *path);
void FCA_html_post_search_ans(const char *query);

void FCA_html_prog_end();


FCA_Tps FCA_html_ps;


void FCA_html_init()
{
	FCA_html_ps.prog_begin=FCA_html_prog_begin;
	
	FCA_html_ps.pre_listing=FCA_html_pre_listing;
	FCA_html_ps.pre_search_ans=FCA_html_pre_search_ans;
	
	FCA_html_ps.tab_top=FCA_html_tab_top;
	FCA_html_ps.tab_title=FCA_html_tab_title;
	FCA_html_ps.tab_untitle=FCA_html_tab_untitle;
	
	FCA_html_ps.tab_pre_stitle=FCA_html_tab_pre_stitle;
	FCA_html_ps.tab_stitle=FCA_html_tab_stitle;
	FCA_html_ps.tab_post_stitle=FCA_html_tab_post_stitle;
	
	FCA_html_ps.tab_pre_item=FCA_html_tab_pre_item;
	FCA_html_ps.pre_tab_item=FCA_html_pre_tab_item;
	FCA_html_ps.tab_item=FCA_html_tab_item;
	FCA_html_ps.post_tab_item=FCA_html_post_tab_item;
	FCA_html_ps.tab_post_item=FCA_html_tab_post_item;
	
	FCA_html_ps.tab_btm=FCA_html_tab_btm;

	FCA_html_ps.infos=FCA_html_infos;
	FCA_html_ps.num=FCA_html_num;
	FCA_html_ps.info_size=FCA_html_info_size;
	FCA_html_ps.main_num=FCA_html_main_num;
	FCA_html_ps.post_infos=FCA_html_post_infos;
	
	FCA_html_ps.size=FCA_html_size;
	
	FCA_html_ps.pre_err=FCA_html_pre_err;
	FCA_html_ps.post_err=FCA_html_post_err;

	FCA_html_ps.pre_warning=FCA_html_pre_warning;
	FCA_html_ps.post_warning=FCA_html_post_warning;

	FCA_html_ps.pre_ok=FCA_html_pre_ok;
	FCA_html_ps.post_ok=FCA_html_post_ok;

	FCA_html_ps.pre_serv=FCA_html_pre_serv;
	FCA_html_ps.post_serv=FCA_html_post_serv;

	FCA_html_ps.pre_dir=FCA_html_pre_dir;
	FCA_html_ps.post_dir=FCA_html_post_dir;

	FCA_html_ps.pre_file=FCA_html_pre_file;
	FCA_html_ps.post_file=FCA_html_post_file;

	FCA_html_ps.pre_file_exec=FCA_html_pre_file_exec;
	FCA_html_ps.post_file_exec=FCA_html_post_file_exec;
	
	FCA_html_ps.pre_path=FCA_html_pre_path;
	FCA_html_ps.post_path=FCA_html_post_path;

	FCA_html_ps.post_listing=FCA_html_post_listing;
	FCA_html_ps.post_search_ans=FCA_html_post_search_ans;
	
	FCA_html_ps.prog_end=FCA_html_prog_end;
}


void FCA_html_prog_begin()
{
if(!FCA_VAR_IS_ON(FCA_html_included_doc))
printf("<html>
<head>
 <title>ffss client</title>
</head>
 
<body>
");
printf("
<form name='browsing' action='");FCA_my_url(false);printf("' method=GET>");
printf("<center>\n");
printf(" <a href='javascript:history.back()'>&lt;-</a>&nbsp;\n");
printf(" ");FCA_dir_link("/$");printf("home</a>&nbsp;\n");
printf(" <a href='javascript:history.forward()'>-&gt;</a>\n");
printf("</center>\n");
FCA_form_hidden_args(true);
}

void FCA_html_pre_listing(char *path)
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
<form name='search' action='");FCA_my_url(false);printf("' method=GET>");
FCA_form_hidden_args(false);printf("
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
  ");FCA_sep_link(path, "");printf("
 </h2>
</center>
");
}

void FCA_html_pre_search_ans(const char *query)
{
	char *p;
	const FCA_Tskin *ps;

printf("address: <input type='text' name='dir' value='/$' tabindex=1>
&nbsp;<input type='submit' value='go'><br>\n");printf("
<input type='hidden' name='s' value=''>
<input type='hidden' name='sdom' value=''>
</form>
<form name='search' action='");FCA_my_url(false);printf("' method=GET>");
FCA_form_hidden_args(true);printf("
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
");
}

void FCA_html_tab_top()
{
	printf("<center>\n<table width='100%%' border=1>\n");
}

void FCA_html_tab_title(const char title[])
{

	printf(" <tr><td>\n");
	printf("  <h2>%s</h2>\n", title);
	printf(" </td></tr>\n");
}

void FCA_html_tab_untitle()
{
	printf("</table>\n<table width='100%%' border=1>\n");
}

void FCA_html_tab_pre_stitle()
{
	printf(" <tr>\n");
}

void FCA_html_tab_stitle(const char name[], const unsigned int size)
{
	printf("  <td><b>%s</b></td>\n", name);
}

void FCA_html_tab_post_stitle()
{
	printf(" </tr>\n");
}

void FCA_html_tab_pre_item()
{
	printf(" <tr>\n");
}

void FCA_html_pre_tab_item()
{
	printf("  <td>");
}

void FCA_html_tab_item(const char name[], const unsigned int size)
{
	printf("%s", name);
}

void FCA_html_post_tab_item()
{
	printf("</td>\n");
}

void FCA_html_tab_post_item()
{
	printf(" </tr>\n");
}

void FCA_html_tab_btm()
{
	printf("</table>\n</center><br>\n");
}

void FCA_html_infos(const char format[], ...)
{
	va_list argptr;
	
	va_start(argptr,format);
	vprintf(format, argptr);
	va_end(argptr);
}

void FCA_html_num(int n, const char text[])
{
	FCA_print_nb(n, text);
}

void FCA_html_info_size(FFSS_LongField n, const char text[])
{
	FCA_print_size(n, (char*)text);
}

void FCA_html_main_num(int n, const char text[])
{
	printf("<b>");
	FCA_print_nb(n, text);
	printf("</b>");
}

void FCA_html_post_infos()
{
	printf("<br>\n</center>\n");
}

void FCA_html_size(FFSS_LongField n, const char text[])
{
	FCA_print_size(n, (char*)text);
}

void FCA_html_pre_err()
{
	fprintf(FCA_err_stream, "<font color=red><b>");
}

void FCA_html_post_err()
{
	fprintf(FCA_err_stream, "</b></font><br>\n");
}

void FCA_html_pre_warning()
{
	printf("<font color=orange><b>");
}

void FCA_html_post_warning()
{
	printf("</b></font><br>\n");
}


void FCA_html_pre_ok()
{
	printf("<font color=green><b>");
}

void FCA_html_post_ok()
{
	printf("</b></font><br>\n");
}

void FCA_html_pre_serv(const char *domain, const char *name, long int state, bool isName)
{
	char all[FFSS_MAX_FILEPATH_LENGTH];

	if(state==FFSS_STATE_OFF)
		printf("<font color=gray>");
	else if(state==FFSS_STATE_QUIET)
		printf("<font color=brown>");
	else
		printf("<font color=black>");
	
	if(isName) {
		if(state!=FFSS_STATE_OFF) {
			snprintf(all, FFSS_MAX_FILEPATH_LENGTH-1, "/$/%s/%s", domain, name);
			FCA_dir_link(all);
		}
	}
}

void FCA_html_post_serv(bool isName)
{
	printf("</font>");
	if(isName)
		printf("</a>");
}

void FCA_html_pre_dir(const char *prefx, const char *name, bool isName)
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

void FCA_html_post_dir(bool isName)
{
	if(isName)
		printf("</a>/");
}

void FCA_html_pre_file(const char *prefx, const char *name, bool isName)
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

void FCA_html_post_file(bool isName)
{
	if(isName)
		printf("</a>");
}

void FCA_html_pre_file_exec(const char *prefx, const char *name, bool isName)
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

void FCA_html_post_file_exec(bool isName)
{
	if(isName)
		printf("</a>*");
}

bool FCA_html_pre_path(const char *domain, const char *path, long int state, bool isName, bool isSamba, bool isDir)
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
				FCA_sep_link(all, dom);
			if(all)
				free(all);
		}
	}
	return true;
}

void FCA_html_post_path(bool isName)
{
	printf("</font>");
	if(isName)
		printf("</a>");
}


void FCA_html_post_listing(char *path)
{

}

void FCA_html_post_search_ans(const char *query)
{

}

void FCA_html_prog_end()
{
printf("<center>\n");
printf(" <a href='javascript:history.back()'>&lt;-</a>&nbsp;\n");
printf(" ");FCA_dir_link("/$");printf("home</a>&nbsp;\n");
printf(" <a href='javascript:history.forward()'>-&gt;</a>\n");
printf("</center>
</form>
");
if(!FCA_VAR_IS_ON(FCA_html_included_doc))
printf("
</body>
</html>
");
}


	/* common to all html skins */
void FCA_dir_link(const char *dir)
{
	FCA_pre_link();
	FCA_dir_arg(dir, false);
	FCA_post_link(false);
}

void FCA_file_link(const char *file)
{
#ifdef CGI_DOWNLOADS
	FCA_pre_link();
	FCA_dir_arg(file, true);
	printf("&download=1");
	FCA_post_link(false);
#endif
}

void FCA_dir_arg(const char *dir, bool isFile)
{
	char *tl;
	
	tl=FCA_cgi_escape_special_chars(dir);
	FCA_my_url(isFile);
	printf("%cdir=%s",
		FCA_VAR_IS_ON(FCA_html_firstarg)?'?':'&',
		tl);
	free(tl);
}

void FCA_pre_link()
{
	printf("<a href='");
}

void FCA_post_link(bool firstArg)
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
	printf("'>");
}

void FCA_form_hidden_args(bool canskin)
{
	char *p;
	
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

void FCA_my_url(bool isDownload)
{
	if(isDownload)
		printf("%s",
			FCA_html_dw_prefix
		);
	else
		printf("%s",
			FCA_html_prefix
		);
}

void FCA_sep_link(char *path, char *prefx)
{
	char *p, *begin=path;
	char p2[FFSS_MAX_FILEPATH_LENGTH];
	
	p=strchr(path, '/');
	if(p==path)
		p=strchr(path+1, '/');
	if(!p) {
		FCA_pre_link();
		snprintf(p2, FFSS_MAX_FILEPATH_LENGTH-1, "%s%s", prefx, path);
		FCA_dir_arg(p2, false);
		FCA_post_link(true);
		printf("%s</a>", path);
		return;
	}
	while(p) {
		*p='\0';
		FCA_pre_link();
		snprintf(p2, FFSS_MAX_FILEPATH_LENGTH-1, "%s%s", prefx, path);
		FCA_dir_arg(p2, false);
		FCA_post_link(false);
		printf("%s</a>", begin);
		
		*p='/';
		begin=p+1;
		p=strchr(p+1, '/');
		printf("/");
	}
	FCA_pre_link();
	FCA_dir_arg(path, false);
	FCA_post_link(false);
	printf("%s</a>", begin);
}

void FCA_smb_sep_link(char *path)
{
	char *p, *begin=path;
	
		/* / -> \ */
	p=path;
	while((p=strchr(p, '/')))
		*p='\\';
	p=strchr(path, '\\');
	if(p==path)
		p=strchr(path+1, '\\');
	if(!p) {
		FCA_pre_link();
		printf("\\\\%s'>", path);
		printf("%s</a>", path);
		return;
	}
	while(p) {
		*p='\0';
		FCA_pre_link();
		printf("\\\\%s'>", path);
		printf("%s</a>", begin);
		
		*p='/';
		begin=p+1;
		p=strchr(p+1, '\\');
		printf("/");
	}
	FCA_pre_link();
	printf("\\\\%s'>", path);
	printf("%s</a>", begin);
}
