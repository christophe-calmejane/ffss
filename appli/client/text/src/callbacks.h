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

#ifndef _FCA_CALLBACKS_H_
#define _FCA_CALLBACKS_H_

#include <ffss.h>

    /* UDP */
void FCA_OnNewState(FFSS_Field State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[]);
void FCA_OnBeginTCPThread(SU_PClientSocket Server);
void FCA_OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList,FFSS_LongField User);
void FCA_OnEndServerListingAnswer(void);
void FCA_OnDomainListingAnswer(const char **Domains,int NbDomains,FFSS_LongField User);
void FCA_OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares, FFSS_LongField User);
void FCA_OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,FFSS_LongField *ChkSums,FFSS_LongField *Sizes,int NbAnswers,FFSS_LongField User);
void FCA_OnMasterError(FFSS_Field ErrorCode,const char Descr[]);
    /* TCP */
bool FCA_OnError(SU_PClientSocket Server,FFSS_Field ErrorCode,const char Descr[],FFSS_LongField Value,FFSS_LongField User);
bool FCA_OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries,FFSS_LongField User);
void FCA_OnEndTCPThread(SU_PClientSocket Server);
void FCA_OnIdleTimeout(SU_PClientSocket Client);
void FCA_OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download);
void FCA_OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download);
void FCA_OnTransfertSuccess(FFSS_PTransfer FT,bool Download);
FFSS_PTransfer FCA_OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[],FFSS_Field XFerTag);
FFSS_PTransfer FCA_OnData(SU_PClientSocket Server,FFSS_Field XFerTag);
    /* fatal error */
void FCA_OnFatalError(void);
void FCA_OnUDPError(int ErrNum);

#endif
