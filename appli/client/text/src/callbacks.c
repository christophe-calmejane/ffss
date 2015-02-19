/*
 *	FFSS client
 *
 *	Copyright (C) 2001 bennyben (Benoit Bourdin)
 *
 *	callbacks from the library
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

#include "callbacks.h"
#include "display.h"
#include "client.h"
#include "common.h"
#include "command.h"

void FCA_OnBeginTCPThread(SU_PClientSocket Server)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) Thread is started");
}

    /* new machine found */
void FCA_OnNewState(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
	if(FCA_inDispServs) {
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) received a new state from %s", IP);
		FCA_print_state(State, IP, Domain, Name, OS, Comment, MasterIP);
	}
}

void FCA_OnDomainListingAnswer(const char **Domains,int NbDomains,FFSS_LongField User)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) received a domain listing");
	FCA_print_domains(Domains, NbDomains);
	    	/* end of domains, we stop the domain listing */
	FCA_sem_post();
}

/* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped !!! */
/* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void FCA_OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList,FFSS_LongField User)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) received a server listing for domain %s", Domain);
	FCA_print_servers(Domain, NbHost, HostList);

	/* no sem_post, this callback is called for each domain */
}

void FCA_OnEndServerListingAnswer(void)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) End of server listing");
	FCA_inDispServs=false;
		/* end of domains, we stop the server listing */
	FCA_sem_post();
}

    /* new ls */
bool FCA_OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries,FFSS_LongField User)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) received a dir listing for path %s", Path);
	FCA_print_ls(Path, NbEntries,Entries);

	FCA_sem_post();
	return true;
}
    /* listing shares */
void FCA_OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares, FFSS_LongField User)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) received a share listing from %s", IP);
	FCA_print_shares(IP,Names,Comments,NbShares);

/* only for debug
sleep(5);
printf("POUF\n");
*/
	FCA_sem_post();
}

void FCA_OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,FFSS_LongField *ChkSums,FFSS_LongField *Sizes,int NbAnswers,FFSS_LongField User)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) received search answer for domain %s, query=%s", Domain, Query);

	if(FCA_inDispFind || !FCA_multiFind)
		FCA_print_search(Query,Domain,Answers,ChkSums,Sizes,NbAnswers);
	else
		SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) sorry, too late to give your answer");

    	if(!FCA_multiFind)
		FCA_sem_post();
}

void FCA_OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download)
{
	FCA_dw_amount+=Amount;
	if(!FCA_quiet)
		FCA_progr_bar();
}

void FCA_OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download)
{
	if(!FCA_quiet)
		printf("\n");
	FCA_quiet=false;
	FCA_print_cmd_err("download failed...");
	FCA_print_cmd_err("(error code %d, %s)\n", (int)ErrorCode, Error);
	FCA_Ptrans=NULL;

	FCA_sem_post();
}

void FCA_OnTransfertSuccess(FFSS_PTransfer FT,bool Download)
{
	time_t end;

	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) download ended without any error");
	end=time(NULL);
	if(end-FCA_dw_begin==0)
		end++;
	FCA_print_dw_ok(FCA_dw_file, FCA_dw_amount/(end-FCA_dw_begin));
	FCA_Ptrans=NULL;

/*SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) sleeeeeeeep");
	usleep(2000000);
SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) wake up baby");
*/
	FCA_sem_post();
}

void FCA_OnMasterError(FFSS_Field ErrorCode,const char Descr[])
{
	FCA_print_cmd_err("error from master (code %d, %s)\n", ErrorCode, Descr);

	FCA_sem_post();
}

    /* error */
bool FCA_OnError(SU_PClientSocket Server,FFSS_Field ErrorCode,const char Descr[],FFSS_LongField Value,FFSS_LongField User)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) error message recieved: Code: %d (%s, value=%d)", ErrorCode, Descr, ErrorCode==FFSS_ERROR_PROTOCOL_VERSION_ERROR?Value:0);
	if(ErrorCode==FFSS_ERROR_XFER_MODE_NOT_SUPPORTED) {
		FCA_print_warning("This server supports only useconnsock mode, switching to this one");
		sprintf(FCA_useConnSock, "on");
		FCA_err_errno=ErrorCode;
	} else {
		if(! FCA_quiet)
			FCA_print_conn_err(Server, ErrorCode, Descr);
		if(ErrorCode!=FFSS_ERROR_NO_ERROR) {	/* if error during share connection */
			/* connection altered */
			if( ErrorCode!=FFSS_ERROR_FILE_NOT_FOUND && ErrorCode!=FFSS_ERROR_ACCESS_DENIED )
				FCA_close_connection();
			FCA_err_errno=ErrorCode;
		}
	}
	FCA_sem_post();
	return true;
}

    /* connection lost */
void FCA_OnEndTCPThread(SU_PClientSocket Server)
{
	char *domain, *machine, *share, *dir;

	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) connection closed to this share, thread stopped");
	FCA_close_connection();
	if(FCA_canChange_pwd) {
			/* update pwd, and go on /$/domain/machine */
		FCA_explode_path(FCA_pwd, &domain, &machine, &share, &dir);
		if(domain!=NULL && machine!=NULL)
			*(machine-1)='/';
	}

	FCA_sem_post();
}
    /* too many UDP errors: fatal error, exits */
void FCA_OnFatalError(void)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) too many UDP errors");
	if(!FCA_exiting)
		FCA_crash("too many UDP errors, exiting");
}

void FCA_OnUDPError(int ErrNum)
{
	SU_DBG_PrintDebug(FC_DBGMSG_GLOBAL, "(client) UDP error code %d", ErrNum);
	FCA_UDP_errno=ErrNum;

	FCA_sem_post();
}

void FCA_OnIdleTimeout(SU_PClientSocket Client)
{
	FCA_print_cmd_err("IDLE timeout, disconnected");

	FCA_sem_post();
}

FFSS_PTransfer FCA_OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[],FFSS_Field XFerTag)
{
	return FCA_Ptrans;
}

FFSS_PTransfer FCA_OnData(SU_PClientSocket Server,FFSS_Field XFerTag)
{
	return FCA_Ptrans;
}
