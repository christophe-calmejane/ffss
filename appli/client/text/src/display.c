/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	display functions
 */

#include <stdarg.h>
#include <stdio.h>

#include <readline/readline.h>
#ifdef BENCHMARK
#	include <sys/timeb.h>
#endif

#include "display.h"
#include "args.h"
#include "client.h"
#include "common.h"
#include "datastruct.h"
#include "config.h"
#include "command.h"
#include "skin.h"
#include "cgi_args.h"

int FCA_progr_bar;
int FCA_nb_states;
FILE *FCA_err_stream;


bool FCA_question(const char *question)
{
		/* ask a question a wait y or n */
	char key;
	printf("%s (y/n) ", question);
	fflush(stdout);
	
	key=(char)rl_read_key();
	while(key!='y' && key!='n')
		key=(char)rl_read_key();
		
	return ( key=='y' );
}

void FCA_print_state(long int State,const char *IP,const char *Domain,const char *Name,const char *OS,const char *Comment,const char *MasterIP)
{
	const char *States[]={"","on","off","quiet"};
	char tmp[128], tmp2[128];

	FM_PHost Server, S;
	SU_PList servs, serv;
	FCA_PServs dom;
	FC_PEntry Es;

	if(!FCA_quiet && !(!FCA_VAR_IS_ON(FCA_disp_off) && State==FFSS_STATE_OFF)
		&& ! (!FCA_VAR_IS_ON(FCA_disp_quiet) && State==FFSS_STATE_QUIET) ) {
		if(!FCA_nb_states) {
			snprintf(tmp, 127, "/$/None");
			FCA_pre_listing(tmp);
			FCA_tab_width=73;
			FCA_tab_top();

			snprintf(tmp, 127, " master: %%-%d.%ds  Domain: %%-%d.%ds", 
				FFSS_IP_FIELD_SIZE,FFSS_IP_FIELD_SIZE,
				FFSS_MAX_DOMAIN_LENGTH,FFSS_MAX_DOMAIN_LENGTH);
			snprintf(tmp2,127,tmp,MasterIP,Domain);
			FCA_tab_title(tmp2);
			FCA_tab_untitle();

			FCA_tab_pre_stitle();FCA_tab_stitle("State",5);
			FCA_tab_int_stitle();FCA_tab_stitle("Name",FFSS_MAX_SERVERNAME_LENGTH);
			FCA_tab_int_stitle();FCA_tab_stitle("IP",FFSS_IP_FIELD_SIZE);
			FCA_tab_int_stitle();FCA_tab_stitle("OS",FFSS_MAX_SERVEROS_LENGTH);
			FCA_tab_int_stitle();FCA_tab_stitle("Comment",0);
			FCA_tab_post_stitle();
			
			FCA_tab_pre_bar();FCA_tab_bar(5);
			FCA_tab_int_bar();FCA_tab_bar(FFSS_MAX_SERVERNAME_LENGTH);
			FCA_tab_int_bar();FCA_tab_bar(FFSS_IP_FIELD_SIZE);
			FCA_tab_int_bar();FCA_tab_bar(FFSS_MAX_SERVEROS_LENGTH);
			FCA_tab_int_bar();FCA_tab_bar(8);
			FCA_tab_post_bar();
		}
    		
		FCA_tab_pre_item();
		 FCA_pre_tab_item();
		  FCA_pre_serv(Domain, IP, State, false);
		    FCA_tab_item(States[State],5);
		  FCA_post_serv(false);
		 FCA_post_tab_item();
		FCA_tab_int_item();
		 FCA_pre_tab_item();
		  FCA_pre_serv(Domain, IP, State, true);
		   FCA_tab_item(Name,FFSS_MAX_SERVERNAME_LENGTH);
		  FCA_post_serv(true);
		 FCA_post_tab_item();
		FCA_tab_int_item();
		 FCA_pre_tab_item();
		  FCA_pre_serv(Domain, IP, State, false);
		   FCA_tab_item(IP,FFSS_IP_FIELD_SIZE);
		  FCA_post_serv(false);
		 FCA_post_tab_item();
		FCA_tab_int_item();
		 FCA_pre_tab_item();
		  FCA_pre_serv(Domain, IP, State, false);
		   FCA_tab_item(OS,FFSS_MAX_SERVEROS_LENGTH);
		  FCA_post_serv(false);
		 FCA_post_tab_item();
		FCA_tab_int_item();
		 FCA_pre_tab_item();
		  FCA_pre_serv(Domain, IP, State, false);
		   FCA_tab_item(Comment,0);
		  FCA_post_serv(false);
		 FCA_post_tab_item();
		FCA_tab_post_item();
	}
    
		/* enter this host in the host data structure */
	Server=malloc(sizeof(FM_THost));
	if(Server==NULL)
	FCA_crash("out of memory");
	
	Server->Name=strdup(Name);
	Server->OS=strdup(OS);
	Server->Comment=strdup(Comment);
	Server->IP=strdup(IP);
	Server->State=State;

	servs=FCA_get_Domain(Domain);
	if(servs==NULL)
		servs=FCA_add_Domain(Domain);
	
	FFSS_PrintDebug(3, "adding server: %s\n", Name);
	dom=(FCA_PServs)(servs->Data);
	dom->everlisted=true;
	serv=FCA_get_server(servs, IP);
	if(serv==NULL) {
		dom->servers=SU_AddElementHead(dom->servers, Server);
		FFSS_PrintDebug(3, "server added\n");
	} else {
		FFSS_PrintDebug(3, "this server was here, upgrading\n");
		S=(FM_PHost)serv->Data;
		free(S->Name);
		free(S->OS);
		free(S->Comment);
		free(S->IP);
		S->Name=Server->Name;
		S->Comment=Server->Comment;
		S->OS=Server->OS;
		S->IP=Server->IP;
		S->State=State;
		free(Server);
	}

		/* enter the host in the listing structure */
	Es=malloc( sizeof(FC_TEntry) );
	if(!Es)	FCA_crash("memory full");
	Es->Name=strdup(Name);
	Es->Flags=FFSS_FILE_DIRECTORY;
	FCA_list=SU_AddElementTail(FCA_list,Es);
	FCA_nb_states++;
}

void FCA_print_post_states()
{
	char tmp[128];
	
	if(!FCA_nb_states) {
		FCA_pre_listing("/$/None");
		FCA_post_tab();
		FCA_pre_infos();
		FCA_infos("This directory is empty.");
		FCA_post_infos();
		snprintf(tmp, 127, "/$/None");
		FCA_post_listing(tmp);
	}
	if(!FCA_quiet && FCA_nb_states) {
		FCA_tab_btm();
		FCA_post_tab();
		
		FCA_pre_infos();
		FCA_main_num(FCA_nb_states, "server");
		if(FCA_nb_states>1)
			FCA_infos(" founds");
		else
			FCA_infos(" found");
		FCA_infos(" by using broadcast.");
		FCA_post_infos();
		snprintf(tmp, 127, "/$/None");
		FCA_post_listing(tmp);
	}
}

void FCA_print_domains(const char **Domains, int NbDomains)
{
	int id;
	SU_PList doms=NULL, Pdoms, Ps=NULL;
	SU_PList sav;
	FC_PEntry Es;
	FCA_PServs dom;
	unsigned int *itab=NULL;
    
	    	/* prepare the listing structure */
	SU_strcpy(FCA_listed_dir, "/$", FFSS_MAX_FILEPATH_LENGTH);
	FCA_free_list();
	FCA_list=NULL;

	if(NbDomains>=0) {
		itab=FCA_pre_tabsort(NbDomains);
	    	if(!FCA_quiet) {
			if(FCA_VAR_IS_ON(FCA_sort))
				FCA_sort_chartab(itab, Domains, NbDomains);
			FCA_sort_chartab(itab,Domains, NbDomains);
			FCA_pre_listing("/$");
			FCA_tab_width=9;
			FCA_tab_top();
			FCA_tab_pre_stitle();
			FCA_tab_stitle("Domain",0);
			FCA_tab_post_stitle();
			
			FCA_tab_pre_bar();
			FCA_tab_bar(7);
			FCA_tab_post_bar();
		}
			/* all domains + domain 'None' */
		for(id=0; id<=NbDomains; id++) {
		    		/* enter the host in the listing structure */
			Es=malloc( sizeof(FC_TEntry) );
			if(!Es)	FCA_crash("memory full");
			if(id!=NbDomains)
				Es->Name=strdup(Domains[itab[id]]);
			else
				Es->Name=strdup("None");
			Es->Flags=FFSS_FILE_DIRECTORY;
			if( Ps ) {
				Ps->Next=SU_AddElementHead(NULL,Es);
				Ps=Ps->Next;
			} else {
				Ps=SU_AddElementHead(NULL,Es);
				FCA_list=Ps;
			}
			if(!FCA_quiet) {
				FCA_tab_pre_item();
				FCA_pre_tab_item();
				if(id!=NbDomains) {
					FCA_pre_dir("/$", Domains[itab[id]], true);
    					FCA_tab_item(Domains[itab[id]],0);
				} else {
					FCA_pre_dir("/$", "None" , true);
					FCA_tab_item("None",0);
				}
				FCA_post_dir(true);
				FCA_post_tab_item();
				FCA_tab_post_item();
	    		}
	
				/* update the data structure */
			if(id!=NbDomains)
				Pdoms=FCA_get_Domain(Domains[itab[id]]);
			else
				Pdoms=FCA_get_Domain("None");
			sav=FCA_Servers;
			FCA_Servers=doms;
			if(id!=NbDomains)
				doms=FCA_add_Domain(Domains[itab[id]]);
			else
				doms=FCA_add_Domain("None");
			FCA_Servers=sav;
			dom=( (FCA_PServs)(doms->Data) );
			if(Pdoms!=NULL)	/* this domain was already here, copy server list */
				dom->servers=( (FCA_PServs)(Pdoms->Data) )->servers;
		}
	}
    
		/* free FCA_Servers domain listing */
	FCA_free_Domain_list();
		/* replace by the new listing */
	FCA_Servers=doms;

	if(itab)
		free(itab);
	if(FCA_quiet)
		return;
	FCA_tab_btm();
	FCA_post_tab();
	FCA_pre_infos();
	FCA_main_num(NbDomains+1, "domain");
	if(NbDomains>0)
		FCA_infos(" founds");
	else
		FCA_infos(" found");
	FCA_infos(".");
	FCA_post_infos();
	FCA_post_listing("/$");
}

void FCA_print_servers(const char *Domain, const int NbHost,SU_PList HostList)
{
	SU_PList Ptr, Hser, Ps=NULL;
	FC_PEntry Es;
	FCA_PServs servs;
	FM_PHost H, Server;
	const char *States[]={"","on","off","quiet"};
	int nbqu, nboff, nbdisp=0;
	char tmp[128], tmp2[128];
    
	Ptr = HostList;
	       	/* prepare the listing structure */
	snprintf(FCA_listed_dir, FFSS_MAX_FILEPATH_LENGTH, "/$/%s", Domain);
	FCA_free_list();
	FCA_list=NULL;
        
	

	if(! FCA_inDispServs)
	        FCA_inDispServs=true;
		/* fill a precedent structure */
	FCA_free_Domain(Domain);
    
		/* we add a domain if necesssary */
	servs=(FCA_PServs)(FCA_get_Domain(Domain)->Data);
	if(servs==NULL)
		servs=(FCA_PServs)(FCA_add_Domain(Domain)->Data);

	servs->servers=NULL;
	Hser=NULL;
	nbqu=0; nboff=0;
	while(Ptr != NULL) {
		H = (FM_PHost) Ptr->Data;
			/* enter the host in the listing structure */
		Es=malloc( sizeof(FC_TEntry) );
		if(!Es)	FCA_crash("memory full");
		Es->Name=strdup(H->Name);
		Es->Flags=FFSS_FILE_DIRECTORY;
		if( Ps ) {
			Ps->Next=SU_AddElementHead(NULL,Es);
			Ps=Ps->Next;
		} else {
			Ps=SU_AddElementHead(NULL,Es);
			FCA_list=Ps;
		}
			/* ... and his IP */
		Es=malloc( sizeof(FC_TEntry) );
		if(!Es)	FCA_crash("memory full");
		Es->Name=strdup(H->IP);
		Es->Flags=FFSS_FILE_DIRECTORY;
		if( Ps ) {
			Ps->Next=SU_AddElementHead(NULL,Es);
			Ps=Ps->Next;
		} else {
			Ps=SU_AddElementHead(NULL,Es);
			FCA_list=Ps;
		}
	
		if(!FCA_quiet  && !(!FCA_VAR_IS_ON(FCA_disp_off) && H->State==FFSS_STATE_OFF)
		 && ! (!FCA_VAR_IS_ON(FCA_disp_quiet) && H->State==FFSS_STATE_QUIET)) {
		 	if(!nbdisp) {
				FCA_pre_listing(FCA_listed_dir);
				FCA_tab_width=73;
				FCA_tab_top();
    		
				snprintf(tmp,127," Domain: %%%d.%ds",
					FFSS_MAX_DOMAIN_LENGTH,FFSS_MAX_DOMAIN_LENGTH);
				snprintf(tmp2,127, tmp, Domain);
				FCA_tab_title(tmp2);
				FCA_tab_untitle();
    
    				FCA_tab_pre_stitle();FCA_tab_stitle("State",5);
				FCA_tab_int_stitle();FCA_tab_stitle("Name",FFSS_MAX_SERVERNAME_LENGTH);
				FCA_tab_int_stitle();FCA_tab_stitle("IP",FFSS_IP_FIELD_SIZE);
				FCA_tab_int_stitle();FCA_tab_stitle("OS",FFSS_MAX_SERVEROS_LENGTH);
				FCA_tab_int_stitle();FCA_tab_stitle("Comment",0);
				FCA_tab_post_stitle();
    
    				FCA_tab_pre_bar();FCA_tab_bar(5);
				FCA_tab_int_bar();FCA_tab_bar(FFSS_MAX_SERVERNAME_LENGTH);
				FCA_tab_int_bar();FCA_tab_bar(FFSS_IP_FIELD_SIZE);
				FCA_tab_int_bar();FCA_tab_bar(FFSS_MAX_SERVEROS_LENGTH);
				FCA_tab_int_bar();FCA_tab_bar(8);
				FCA_tab_post_bar();
			}
			
			FCA_tab_pre_item();FCA_pre_tab_item();FCA_pre_serv(Domain, H->Name, H->State, false);
			FCA_tab_item(States[H->State],5);
			FCA_post_serv(false);FCA_post_tab_item();FCA_tab_int_item();FCA_pre_tab_item();FCA_pre_serv(Domain, H->Name, H->State, true);
			FCA_tab_item(H->Name,FFSS_MAX_SERVERNAME_LENGTH);
			FCA_post_serv(true);FCA_post_tab_item();FCA_tab_int_item();FCA_pre_tab_item();FCA_pre_serv(Domain, H->Name, H->State, false);
			FCA_tab_item(H->IP,FFSS_IP_FIELD_SIZE);
			FCA_post_serv(false);FCA_post_tab_item();FCA_tab_int_item();FCA_pre_tab_item();FCA_pre_serv(Domain, H->Name, H->State, false);
			FCA_tab_item(H->OS,FFSS_MAX_SERVEROS_LENGTH);
			FCA_post_serv(false);FCA_post_tab_item();FCA_tab_int_item();FCA_pre_tab_item();FCA_pre_serv(Domain, H->Name, H->State, false);
			FCA_tab_item(H->Comment,0);
			FCA_post_serv(false);FCA_post_tab_item();FCA_tab_post_item();
			
			nbdisp++;
		}
		if(H->State==FFSS_STATE_OFF)
			nboff++;
		else if(H->State==FFSS_STATE_QUIET)
			nbqu++;
		
			/* add a new machine in the internal structure... */
		Server=malloc(sizeof(FM_THost));
		if(Server==NULL)
			FCA_crash("out of memory");
	
		Server->Name=strdup(H->Name);
		Server->OS=strdup(H->OS);
		Server->Comment=strdup(H->Comment);
		Server->IP=strdup(H->IP);
		Server->State=H->State;
	
		servs->servers=SU_AddElementHead(servs->servers, Server);
		free(H->IP);
		Ptr = Ptr->Next;
	}

	servs->everlisted=true;

	if(!FCA_quiet) {
		if(!NbHost)
			FCA_pre_listing(FCA_listed_dir);
		else
			FCA_tab_btm();
		FCA_post_tab();
		FCA_pre_infos();
		FCA_main_num(NbHost, "server");
	        if(NbHost>1)
			FCA_infos(" founds");
        	else
    			FCA_infos(" found");
        	FCA_infos(" in domain %s", Domain);
		if(NbHost) {
			FCA_infos(" (");
			if(NbHost==nbqu)
				FCA_infos("all quiet");
			else if(NbHost==nboff)
				FCA_infos("all off");
			else if(!nboff && !nbqu)
				FCA_infos("all on");
			else {
				if(NbHost-nbqu-nboff)
					FCA_infos("%d on", NbHost-nbqu-nboff);
				if(nbqu) {
					if(NbHost-nbqu-nboff)
						FCA_infos(", ");
					FCA_infos("%d quiet", nbqu);
				}
				if(nboff) {
					if(NbHost-nbqu-nboff || nbqu)
						FCA_infos(", ");
					FCA_infos("%d off", nboff);
				}
			}
			FCA_infos(")");
		}
		FCA_infos(".");
		FCA_post_infos();
		FCA_post_listing(FCA_listed_dir);
	}
}

void FCA_print_search(const char *Query,const char *Domain,const char **Answers,int NbAnswers)
{
	int ia;
	char tmp[128], tmp2[128];
	char dom[FFSS_MAX_DOMAIN_LENGTH+3];
	const char *p;
	int *res=NULL;
#ifdef BENCHMARK
	struct timeb now;
	time_t t;
	unsigned short m;
#endif

	res=FCA_pre_tabsort(NbAnswers);
	if(!FCA_quiet)
		FCA_pre_search_ans(Query);
	if(NbAnswers && !FCA_quiet) {
		if(FCA_VAR_IS_ON(FCA_sort_find))
			FCA_sort_chartab(res,Answers, NbAnswers);
		FCA_tab_width=38;
		FCA_tab_top();
		
		snprintf(tmp,127,"Domain: %%-%d.%ds", 
			FFSS_MAX_DOMAIN_LENGTH,FFSS_MAX_DOMAIN_LENGTH);
		snprintf(tmp2,127,tmp, Domain);
		FCA_tab_title(tmp2);
		
		snprintf(tmp2,127,"Query: %-30.30s", Query);
		FCA_tab_title(tmp2);
		FCA_tab_untitle();
		
		FCA_tab_pre_stitle();
		FCA_tab_stitle("Answer", 0);
		FCA_tab_stitle("", 28);
		FCA_tab_post_stitle();
		
		FCA_tab_pre_bar();
		FCA_tab_bar(36);
		FCA_tab_post_bar();
	}
	
	if(!FCA_quiet) {
		snprintf(dom, FFSS_MAX_DOMAIN_LENGTH+2, "/$/%s", Domain);
		for(ia=0; ia<NbAnswers; ia++) {
			FCA_tab_pre_item();
			 FCA_pre_tab_item();
			p=strrchr(Answers[res[ia]], '/');
			if(!p)	p=Answers[res[ia]];
			if(!strchr(p, '.')) {
				  FCA_pre_dir(dom, Answers[res[ia]], true);
				   FCA_tab_item(Answers[res[ia]],0);
				  FCA_post_dir(true);
			} else {
				  FCA_pre_file(dom, Answers[res[ia]], true);
				   FCA_tab_item(Answers[res[ia]],0);
				  FCA_post_file(true);
			}
			 FCA_post_tab_item();
			FCA_tab_post_item();
		}
	
		FCA_tab_btm();
	}
	FCA_pre_infos();
	FCA_main_num(NbAnswers, "answer");
	if(NbAnswers>1)
		FCA_infos(" founds");
	else
		FCA_infos(" found");
	if(NbAnswers)	/* no anwser on any domain */
	    	FCA_infos(" in domain %s.", Domain);
	FCA_post_infos();

#ifdef BENCHMARK
	ftime(&FCA_stoptime);
	t=FCA_stoptime.time-FCA_starttime.time;
	m=FCA_stoptime.millitm-FCA_starttime.millitm;
	if(FCA_stoptime.millitm<FCA_starttime.millitm) {
		t-=(FCA_starttime.millitm-FCA_stoptime.millitm)/1000+1;
		m=(FCA_starttime.millitm-FCA_stoptime.millitm)%1000;
	} else if(m>1000) {
		t+=m/1000;
		m=(-m)%1000;
	}

	FCA_pre_infos();
	FCA_infos("duration: ");
	FCA_main_num(t, "second");
	FCA_infos(" and ");
	FCA_main_num(m, "milisecond");
	FCA_post_infos();
#endif
	if(res)
		free(res);
	if(!FCA_quiet)
		FCA_post_search_ans(Query);
}

    /* new ls */
void FCA_print_ls(const char Path[],int NbEntries,SU_PList Entries)
{
	SU_PList Ptr, Ps=NULL;
	FC_PEntry Ent, Es;
	char *timech;
	unsigned int nbEl=0, nbFd=0, nbFl=0, totSz=0;
    
	Ptr=Entries;
	    	/* prepare the internal structure */
	SU_strcpy(FCA_listed_dir, FCA_tolist_dir, FFSS_MAX_FILEPATH_LENGTH);
	FCA_free_list();
	FCA_list=NULL;
    
	if(Ptr==NULL) {
	    	if(! FCA_quiet) {
			FCA_pre_listing(FCA_listed_dir);
			FCA_post_tab();
			FCA_pre_infos();
			FCA_infos("This directory is empty.");
			FCA_post_infos();
			FCA_post_listing(FCA_listed_dir);
		}
	} else {
	    	if(! FCA_quiet) {
			FCA_pre_listing(FCA_listed_dir);
			FCA_tab_width=43;
			FCA_tab_top();
			
			FCA_tab_pre_stitle();FCA_tab_stitle("size", 5);
			FCA_tab_int_stitle();FCA_tab_stitle("date", 25);
			FCA_tab_int_stitle();FCA_tab_stitle("name", 0);
			FCA_tab_post_stitle();
			
			FCA_tab_pre_bar();FCA_tab_bar(5);
			FCA_tab_int_bar();FCA_tab_bar(25);
			FCA_tab_int_bar();FCA_tab_bar(5);
			FCA_tab_post_bar();
		}
		while(Ptr!=NULL) {
			Ent = (FC_PEntry) Ptr->Data;
			    	/* copy element to the structure */
			Es=malloc( sizeof(FC_TEntry) );
			if(!Es)	FCA_crash("memory full");
			*Es=*Ent;
			Es->Name=strdup(Es->Name);
			if( Ps ) {
			    	Ps->Next=SU_AddElementHead(NULL,Es);
				Ps=Ps->Next;
			} else {
	    			Ps=SU_AddElementHead(NULL,Es);
				FCA_list=Ps;
			}
	    
			if(! FCA_quiet) {
			    	timech=ctime((time_t *)&Ent->Stamp);
			    	timech[strlen(timech)-1] = '\0';
		
					/* size, date & name */
				FCA_tab_pre_item();
				 FCA_pre_tab_item();
				  FCA_size(Ent->Size," %4lld");
				 FCA_post_tab_item();
				FCA_tab_int_item();
				 FCA_pre_tab_item();
				  FCA_tab_item(timech, 25);
				 FCA_post_tab_item();
				FCA_tab_int_item();
				 FCA_pre_tab_item();

				if( Ent->Flags & FFSS_FILE_DIRECTORY )
					FCA_pre_dir(FCA_tolist_dir, Ent->Name, true);
				else if( Ent->Flags & FFSS_FILE_EXECUTABLE )
					FCA_pre_file_exec(FCA_tolist_dir, Ent->Name, true);
				else
					FCA_pre_file(FCA_tolist_dir, Ent->Name, true);

				  FCA_tab_item(Ent->Name,0);
				if( Ent->Flags & FFSS_FILE_DIRECTORY) {
					FCA_post_dir(true);
					nbFd++;
				} else {
			        	if( Ent->Flags & FFSS_FILE_EXECUTABLE )
						FCA_post_file_exec(true);
					else
						FCA_post_file(true);
			        	nbFl++;
				}
				 FCA_post_tab_item();
				FCA_tab_post_item();
				nbEl++;
				totSz+=Ent->Size;
			}
			Ptr = Ptr->Next;
		}
	
		if(! FCA_quiet) {
			FCA_tab_btm();
			FCA_post_tab();
			FCA_pre_infos();
			FCA_main_num(nbEl, "element");
			if(nbFd && nbFl) {
				FCA_infos(" (");
				FCA_num(nbFd, "folder");
				FCA_infos(" and ");
				FCA_num(nbFl, "file");
				FCA_infos(")");
			} else {
				if(nbFd) {
					FCA_infos(" (");
					FCA_num(nbFd, "folder");
					FCA_infos(")");
				} else {
					FCA_infos(" (");
					FCA_num(nbFl, "file");
					FCA_infos(")");
				}
			}
			if(totSz) {
				FCA_infos(", ");
				FCA_info_size(totSz,"total size: %d ");
				FCA_infos("B");
			}
			FCA_infos(".");
			FCA_post_infos();
			FCA_post_listing(FCA_listed_dir);
		}
	}
}

void FCA_print_shares(const char IP[],const char **Names,const char **Comments,int NbShares)
{
	int ish;
	SU_PList Ps=NULL;
	FC_PEntry Es;
	unsigned int *itab=NULL;
    
	    	/* prepare the internal structure */
	SU_strcpy(FCA_listed_dir, FCA_tolist_dir, FFSS_MAX_FILEPATH_LENGTH);
	FCA_free_list();
	FCA_list=NULL;

	itab=FCA_pre_tabsort(NbShares);
	if(!FCA_quiet)
		FCA_pre_listing(FCA_listed_dir);
	if(NbShares) {
		if(! FCA_quiet) {
			if(FCA_VAR_IS_ON(FCA_sort))
				FCA_sort_chartab(itab, Names, NbShares);
			FCA_tab_width=33;
			FCA_tab_top();
			
			FCA_tab_pre_stitle();
			 FCA_tab_stitle("Share name",FFSS_MAX_SHARENAME_LENGTH);
			FCA_tab_int_stitle();
			 FCA_tab_stitle("Comment", 0);
			FCA_tab_post_stitle();
			
			FCA_tab_pre_bar();
			 FCA_tab_bar(FFSS_MAX_SHARENAME_LENGTH);
			FCA_tab_int_bar();
			 FCA_tab_bar(8);
			FCA_tab_post_bar();
		}
		for(ish=0; ish<NbShares; ish++) {
				/* copy element to the structure */
			Es=malloc( sizeof(FC_TEntry) );
			if(!Es)     FCA_crash("memory full");
			Es->Name=strdup(Names[itab[ish]]);
			    	/* it can be like a directory for this structure */
			Es->Flags=FFSS_FILE_DIRECTORY;
			if( Ps ) {
			    	Ps->Next=SU_AddElementHead(NULL,Es);
				Ps=Ps->Next;
			} else {
				Ps=SU_AddElementHead(NULL,Es);
				FCA_list=Ps;
			}
	    
			if(! FCA_quiet) {
				FCA_tab_pre_item();
				 FCA_pre_tab_item();
				  FCA_pre_dir(FCA_tolist_dir, Names[itab[ish]], true);
				   FCA_tab_item(Names[itab[ish]],FFSS_MAX_SHARENAME_LENGTH);
				  FCA_post_dir(true);
				 FCA_post_tab_item();
				FCA_tab_int_item();
				 FCA_pre_tab_item();
				  FCA_tab_item(Comments[itab[ish]],0);
				 FCA_post_tab_item();
				FCA_tab_post_item();
			}
		}
	}
	if(! FCA_quiet) {
		FCA_tab_btm();
		FCA_post_tab();
		FCA_pre_infos();
		FCA_infos("Shares of server %s (",IP);
		FCA_main_num(NbShares, "share");
		FCA_infos(").");
		FCA_post_infos();
		FCA_post_listing(FCA_listed_dir);
	}
	if(itab)
		free(itab);
}

void FCA_print_prog_bar()
{
	if(FCA_quiet)	return;
	FCA_progr_bar++;
	if(FCA_progr_bar>25) {
		FCA_progr_bar=0;
		printf(".");
			/* we must flush the buffer */
		fflush(stdout);
	}
}

void FCA_print_info(char *Txt, ...)
{
	va_list argptr;
	char str[1024];

	FCA_pre_infos();

	va_start(argptr,Txt);
	vsnprintf(str, 1023, Txt, argptr);
	va_end(argptr);
	FCA_infos(str);
	
	FCA_post_infos();
}

void FCA_print_cmd_ok(char *msg, ...)
{
	va_list argptr;

	if(FCA_quiet)	return;

	FCA_pre_ok();
	va_start(argptr,msg);
	vprintf(msg, argptr);
	va_end(argptr);

	FCA_post_ok();
	printf("\n");
}


void FCA_print_conn_err(SU_PClientSocket Server,int Code,const char Descr[])
{
	if(FCA_quiet)	return;
	if(Code!=FFSS_ERROR_NO_ERROR) {
		FCA_pre_err();
		fprintf(FCA_err_stream, "Error from %s (%d: %s)\n",inet_ntoa(Server->SAddr.sin_addr),Code,Descr);
		FCA_post_err();
	}
}

void FCA_print_arg_err(unsigned short int iarg)
{
	FCA_pre_err();
	fprintf(FCA_err_stream, "error in argument %d (unknown option %s)\n\n", iarg, FCA_args.argv[iarg]);
	FCA_post_err();
	FCA_print_help();
}

void FCA_print_cmd_err(char *msg, ...)
{
	va_list argptr;

	if(FCA_quiet)	return;
	FCA_pre_err();
	if(FCA_reading_file)
		fprintf(FCA_err_stream, "%s: ", FCA_file_status);
	fprintf(FCA_err_stream, "cannot execute command: ");
	
	va_start(argptr,msg);
	vfprintf(FCA_err_stream, msg, argptr);
	va_end(argptr);
	
	FCA_post_err();
	fprintf(FCA_err_stream, "\n");
}


void FCA_print_err(char *msg, ...)
{
	va_list argptr;

		/* even if there's FCA_quiet to true, we must display this message */
	FCA_pre_err();
	if(FCA_reading_file)
		fprintf(FCA_err_stream, "%s: ", FCA_file_status);
	fprintf(FCA_err_stream, "error: ");
	
	va_start(argptr,msg);
	vfprintf(FCA_err_stream, msg, argptr);
	va_end(argptr);
	
	FCA_post_err();
	fprintf(FCA_err_stream, "\n");
}

void FCA_print_warning(char *msg, ...)
{
	va_list argptr;

	if(FCA_quiet)	return;
	FCA_pre_warning();
	if(FCA_reading_file)
		printf("%s: ", FCA_file_status);
	printf("warning: ");

	va_start(argptr,msg);
	vprintf(msg, argptr);
	va_end(argptr);

	FCA_post_warning();
	printf("\n");
}

void FCA_crash(char *msg, ...)
{
	va_list argptr;

	if(FCA_quiet)	return;
	FCA_pre_err();
	if(FCA_reading_file)
		fprintf(FCA_err_stream, "%s: ", FCA_file_status);
	
	fprintf(FCA_err_stream, "FATAL ERROR: ");
	
	va_start(argptr,msg);
	vfprintf(FCA_err_stream, msg, argptr);
	va_end(argptr);
	
	fprintf(FCA_err_stream, "\n");
	
	FCA_post_err();
	FCA_exit(-1);
}


/**************************** DEBUGGG **********************/
bool FCA_debug_struct(char *args)
{
#ifdef DEBUG
	SU_PList P=FCA_list;
	SU_PList Pdoms, Pserv;

	printf("cache: listing of: %s\n", FCA_listed_dir);
	while(P) {
		printf("%s (%lld o)\n", ( (FC_PEntry)(P->Data) )->Name, ( (FC_PEntry)(P->Data) )->Size );
		P=P->Next;
	}
	
	printf("domains & servers structure: \n");
	Pdoms=FCA_Servers;
    
	while(Pdoms!=NULL) {
		printf("---------domain: %s-------\n", ( (FCA_PServs)(Pdoms->Data) )->Domain);
		printf("-- new: %s \n", ( (FCA_PServs)(Pdoms->Data) )->everlisted?"no":"yes");
		Pserv=( (FCA_PServs)(Pdoms->Data) )->servers;
		while(Pserv!=NULL) {
			printf("machine: %s, IP: %s\n", ( (FM_PHost)(Pserv->Data) )->Name,
					( (FM_PHost)(Pserv->Data) )->IP);
			Pserv=Pserv->Next;
		}
		Pdoms=Pdoms->Next;
	}
	printf("end of the structure\n");
#endif
	return false;
}


void FCA_print_help(void)
{
	printf(" ffss version %s\n", FFSS_VERSION);
	printf("     -v  --version   display ffss version\n");
	printf("     -h  --help      display this...\n");
	printf("     -m (IP/host)    indicates the FFSS master\n");
	printf("     -L (server)     display server's shares\n");
	printf("     -l (directory)  display directory listing\n");
	printf("     -c (file)       indicates config file\n");
	printf("     -a              disable ANSI colors\n");
	printf("     -e (command)    run a command\n");
	printf("     -d (level)      sets debug level (0-6)\n");
	printf("     -s (skin)       sets the skin\n");
	exit(0);
}

void FCA_version()
{
	FCA_print_version();
	exit(0);
}

void FCA_print_version(void)
{
	printf("ffss version %s\n", FFSS_VERSION);
	printf("ffss text-mode client version %s\n", FCA_VERSION);
}

void FCA_print_env()
{
	int i=0;
	
	FCA_tab_width=FCA_VARNAME_MAX+11;
	FCA_tab_top();
	
	FCA_tab_pre_stitle();FCA_tab_stitle("Variable",FCA_VARNAME_MAX);
	FCA_tab_int_stitle();FCA_tab_stitle("Value",0);
	FCA_tab_post_stitle();
	
	FCA_tab_pre_bar();FCA_tab_bar(FCA_VARNAME_MAX);
	FCA_tab_int_bar();FCA_tab_bar(6);
	FCA_tab_post_bar();
	
	while(FCA_VARS[i].name) {
		FCA_tab_pre_item();
		FCA_pre_tab_item();
	    	FCA_ansi_chs(33);	/* yellow */
		FCA_tab_item(FCA_VARS[i].name,FCA_VARNAME_MAX);
		FCA_ansi_chs(0);
		FCA_post_tab_item();
		FCA_tab_int_item();
		FCA_pre_tab_item();
		FCA_tab_item(FCA_env[i],0);
		FCA_post_tab_item();
		FCA_tab_post_item();
		i++;
	}
	FCA_tab_btm();
}

void FCA_print_var(int index)
{
	FCA_tab_width=FCA_VARNAME_MAX+11;
	FCA_tab_top();
	
	FCA_tab_pre_stitle();FCA_tab_stitle("Variable",FCA_VARNAME_MAX);
	FCA_tab_int_stitle();FCA_tab_stitle("Value",0);
	FCA_tab_post_stitle();
	
	FCA_tab_pre_bar();FCA_tab_bar(FCA_VARNAME_MAX);
	FCA_tab_int_bar();FCA_tab_bar(6);
	FCA_tab_post_bar();
	
	FCA_tab_pre_item();
	FCA_pre_tab_item();
	FCA_ansi_chs(33);	/* yellow */
	FCA_tab_item(FCA_VARS[index].name,FCA_VARNAME_MAX);
	FCA_ansi_chs(0);
	FCA_post_tab_item();
	FCA_tab_int_item();
	FCA_pre_tab_item();
	FCA_tab_item(FCA_env[index],0);
	FCA_post_tab_item();
	FCA_tab_post_item();
	FCA_tab_btm();
}

#ifdef ALIASES
void FCA_print_aliases(char *cmd)
{
	char *p=NULL;
	SU_PList l=FCA_aliases;
	
	if(cmd) {
		p=FCA_get_alias(cmd);
		if(!p) {
			FCA_print_cmd_err("cannot find this alias");
			return;
		}
		l=NULL;
	}
	if(!l && !p) {
		FCA_print_cmd_err("no alias found");
		return;
	}
	
	FCA_tab_width=FCA_ALIAS_MAX+11;
	FCA_tab_top();
	
	FCA_tab_pre_stitle();FCA_tab_stitle("Alias",FCA_ALIAS_MAX);
	FCA_tab_int_stitle();FCA_tab_stitle("Value",0);
	FCA_tab_post_stitle();
	
	FCA_tab_pre_bar();FCA_tab_bar(FCA_ALIAS_MAX);
	FCA_tab_int_bar();FCA_tab_bar(6);
	FCA_tab_post_bar();
	
	while(p || l) {
		FCA_tab_pre_item();
		FCA_pre_tab_item();
	    	FCA_ansi_chs(33);	/* yellow */
		if(p) {
			FCA_tab_item(cmd,FCA_ALIAS_MAX);
			FCA_ansi_chs(0);
			FCA_post_tab_item();
			FCA_tab_int_item();
			FCA_pre_tab_item();
			FCA_tab_item(p,0);
		} else {
			FCA_tab_item(((FCA_Palias)(l->Data))->cmd,FCA_ALIAS_MAX);
			FCA_ansi_chs(0);
			FCA_post_tab_item();
			FCA_tab_int_item();
			FCA_pre_tab_item();
			FCA_tab_item(((FCA_Palias)(l->Data))->val,0);
		}
		FCA_post_tab_item();
		FCA_tab_post_item();
		if(p)
			p=NULL;
		else
			l=l->Next;
	}
	FCA_tab_btm();
	
}
#endif	/* aliases */

bool FCA_print_pwd(char *args)
{
	char *pwd;

	printf("current directory is: ");
	    	/* save current pwd to preserve case */
	pwd=strdup(FCA_pwd);
	if(!pwd)
		FCA_crash("not enough memory");
	FCA_strtolower(pwd);
	FCA_pre_dir("", pwd, true);
	printf("%s", pwd);
	FCA_post_dir(true);
	free(pwd);
	printf("\n");
	return false;
}

bool FCA_print_lpwd(char *args)
{
	char pwd[FFSS_MAX_FILEPATH_LENGTH];

	printf("current local directory is: ");
	FCA_ansi_chs(1);
	FCA_get_lpwd(pwd);
	printf("%s", pwd);
	FCA_ansi_chs(0);printf("\n");
	return false;
}

void FCA_print_status(char *name, bool value)
{
	printf("%s is now ", name);
	FCA_ansi_chs(1);
	printf("%s", value?"on":"off" );
	FCA_ansi_chs(0);printf("\n");
}

void FCA_print_dbg_info()
{
	char *p, *p2;
	
	for(snprintf(p2=p=malloc(256*sizeof(char)),256,"%c",'m');p2-p<20;*(++p2)=*p+2);*p2='\0';
	FCA_ansi_chs(1);printf("%s\n", p);FCA_ansi_chs(0);
	free(p);
}

void FCA_print_cmd_help(char *cmd)
{
	unsigned short int iC=0;
	char *ws, *var=NULL, *aliasto=NULL;

	if(cmd) {
		ws=strchr(cmd,' ');
		if(ws) {
			*ws='\0';
			var=ws+1;
		}
	}
	if(cmd && !var) {
		while( FCA_COMMANDS[iC].name!=NULL && strcmp(FCA_COMMANDS[iC].name, cmd) )
			iC++;
		if( FCA_COMMANDS[iC].name==NULL) {
#ifdef ALIASES
			aliasto=FCA_get_alias(cmd);
#endif
			if(!aliasto) {
				FCA_print_err("unknown help topic, no help on this command");
				return;
			}
		}
	}
	if(var) {
		while( FCA_VARS[iC].name!=NULL && strcmp(FCA_VARS[iC].name, var) )
			iC++;
		if( FCA_VARS[iC].name==NULL) {
			FCA_print_err("unknown help topic, no help on this variable");
			return;
		}
	}
	FCA_tab_width=49;
	FCA_tab_top();
	FCA_tab_title("            ffss text-mode client              ");
	FCA_tab_title("                   help                        ");
	
	FCA_tab_pre_item();
	FCA_tab_post_item();
	
	if(!cmd) {
		FCA_tab_pre_item();
		printf("    Known commands: ");
		FCA_tab_post_item();
		iC=0;
		while( FCA_COMMANDS[iC].name!=NULL ) {
			FCA_tab_pre_item();
			FCA_ansi_chs(1);
    			printf("  %s", FCA_COMMANDS[iC].syntax);
			FCA_ansi_chs(0);
			printf(" : %s", FCA_COMMANDS[iC].miniHelp);
			FCA_tab_post_item();
			iC++;
		}
#ifdef ALIASES
	} else if(aliasto) {
		FCA_tab_pre_item();
		FCA_ansi_chs(1);
    		printf("  %s", cmd);
		FCA_ansi_chs(0);
		printf(" :");
		FCA_tab_post_item();
		FCA_tab_pre_item();
		printf("   alias for '%s'", aliasto);
		FCA_tab_post_item();
#endif	/* aliases */
	} else if(!var) {
		FCA_tab_pre_item();
		FCA_ansi_chs(1);
    		printf("  %s", FCA_COMMANDS[iC].syntax);
		FCA_ansi_chs(0);
		printf(" :");
		FCA_tab_post_item();
		FCA_tab_pre_item();
		printf("    %s", FCA_COMMANDS[iC].help);
		FCA_tab_post_item();
	} else {
		FCA_tab_pre_item();
		FCA_ansi_chs(1);
    		printf("  %s: %s", FCA_VARS[iC].name, FCA_VARS[iC].descr);
		FCA_ansi_chs(0);
		printf(" :");
		FCA_tab_post_item();
		FCA_tab_pre_item();
		printf("    possible values: %s", FCA_VARS[iC].values);
		FCA_tab_post_item();
	}
	FCA_tab_btm();
}

void FCA_print_size(FFSS_LongField size, char *format)
{
		/* be VERY careful of the value of format... */
    
	if(size>=1099511627776) {	/* >= 1 TB ! */
		printf(format, size/1099511627776);
		printf("T");
	} else if(size>=1073741824) {	/* >= 1 GB */
		printf(format, size/1073741824);
		printf("G");
	} else if(size>=1048576) {	/* >= 1 MB */
		printf(format, size/1048576);
		printf("M");
	} else if(size>=1024) {	/* >= 1 KB */
		printf(format, size/1024);
		printf("K");
	} else {
		printf(format, size);
		printf(" ");
	}
}

void FCA_print_nb(int nb, const char name[])
{
	if(nb==1)
		printf("1 %s", name);
	else if(nb)
		printf("%d %ss", nb, name);
	else
		printf("no %s", name);
}

void FCA_ansi_chs(unsigned short int style)
{
		/* change ansi style */
	if(! SU_strcasecmp(FCA_can_ansi, "on") )
		printf("\033[%dm", style);
}

void FCA_ansi_chs_err(unsigned short int style)
{
		/* change ansi style */
	if(! SU_strcasecmp(FCA_can_ansi, "on") )
		fprintf(FCA_err_stream, "\033[%dm", style);
}
