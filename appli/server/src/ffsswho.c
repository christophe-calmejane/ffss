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

#include "confapi.h"

#define FFSSWHO_VERSION "1.0-pre7"

bool RequestConns(SU_PClientSocket Client,const char Path[])
{
  SU_PList Conns,Ptr,Ptr2;
  FSCA_PConn Conn;

  Conns = FSCA_RequestConns(Client,Path);
  if(Conns == NULL)
  {
    printf("No connection\n");
    return true;
  }
  printf("%-15s  %-35s  Tranfers  Streamings\n","IP","Host");
  printf("--------------------------------------------------------------------------\n");
  Ptr = Conns;
  while(Ptr != NULL)
  {
    Conn = (FSCA_PConn)Ptr->Data;
    printf("%-15s  %-35s  %-8d  %-3d\n",Conn->IP,SU_NameOfPort(Conn->IP),Conn->NbXfers,Conn->NbStrms);
    if(Conn->Xfers != NULL)
    {
      printf("  Current tranfers : \n");
      Ptr2 = Conn->Xfers;
      while(Ptr2 != NULL)
      {
        printf("    %s : %s%%\n",((FSCA_PFileInfo)Ptr2->Data)->Name,((FSCA_PFileInfo)Ptr2->Data)->Pct);
        Ptr2 = Ptr2->Next;
      }
    }
    if(Conn->Strms != NULL)
    {
      printf("  Current streamings : \n");
      Ptr2 = Conn->Strms;
      while(Ptr2 != NULL)
      {
        printf("    %s : %s%%\n",((FSCA_PFileInfo)Ptr2->Data)->Name,((FSCA_PFileInfo)Ptr2->Data)->Pct);
        Ptr2 = Ptr2->Next;
      }
    }
    if((Conn->Xfers != NULL) || (Conn->Strms != NULL))
      printf("\n");
    Ptr = Ptr->Next;
  }
  printf("\n");
  return true;
}

bool RequestSharesList(SU_PClientSocket Client)
{
  SU_PList Shares,Ptr;
  FSCA_PShareLst Share;

  Shares = FSCA_RequestSharesList(Client);
  if(Shares == NULL)
    return true;
  Ptr = Shares;
  while(Ptr != NULL)
  {
    Share = (FSCA_PShareLst)Ptr->Data;
    if((Share->NbConns+Share->NbXfers) != 0)
    {
      printf("Connections for share \"%s\" : \n",Share->Name);
      if(!RequestConns(Client,Share->Path))
        return false;
    }
    Ptr = Ptr->Next;
  }
  return true;
}


void PrintHelp(void)
{
  printf("Usage : ffsswho [options]\n");
  printf("Options : -h or --help : This\n");
  printf("          -v or --version  : Prints server version and exits\n");
  printf("          -g or --global   : Prints server global infos\n");
  printf("          -c or --conns    : Prints actual connection to the server (and active downloads). [Default]\n");
  printf("          -s <servername> <login> <password> : Connects to the specified server instead of localhost, using specified login and password\n");
  printf("          -j <sharename>   : Ejects everybody from specified share\n");
  exit(0);
}

int main(int argc,char *argv[])
{
  SU_PClientSocket Client;
  char *Server,*Login = NULL,*Pwd = NULL,*ejectfrom = NULL;
  int i;
  bool global_info = false;
  bool conn_info = false;
  SU_PList Plugins;

#ifdef _WIN32
  if(!SU_WSInit(2,2))
  {
    printf("Cannot initialize winsock2 !\n");
    return -1;
  }
#endif /* _WIN32 */
  printf("FFSS Who v%s (c) Christophe Calmejane 2001'03\n",FFSSWHO_VERSION);
  printf("FFSS Server v%s (c) Christophe Calmejane 2001'03\n\n",FFSS_SERVER_VERSION);

  Server = "localhost";
  if(argc != 1)
  {
    i = 1;
    while(i<argc)
    {
      if((strcmp(argv[i],"--help") == 0) || (strcmp(argv[i],"-h") == 0))
        PrintHelp();
      else if((strcmp(argv[i],"--version") == 0) || (strcmp(argv[i],"-v") == 0))
      {
        printf("This is free software; see the source for copying conditions.  There is NO\n");
        printf("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n");
        return 0;
      }
      else if((strcmp(argv[i],"-g") == 0) || (strcmp(argv[i],"--global") == 0))
        global_info = true;
      else if((strcmp(argv[i],"-c") == 0) || (strcmp(argv[i],"--conns") == 0))
        conn_info = true;
      else if((strcmp(argv[i],"-s") == 0) || (strcmp(argv[i],"--server") == 0))
      {
        if((i+3) >= argc)
          PrintHelp();
        Server = argv[++i];
        Login = argv[++i];
        Pwd = argv[++i];
      }
      else if((strcmp(argv[i],"-j") == 0) || (strcmp(argv[i],"--eject") == 0))
      {
        if((i+1) >= argc)
          PrintHelp();
        ejectfrom = argv[++i];
      }
      i++;
    }
  }

  Client = FSCA_Connection(Server);
  if(Client == NULL)
  {
    printf("Cannot connect to %s:%s\n",Server,FFSS_SERVER_CONF_PORT_S);
    return -2;
  }
  if(Login != NULL) /* Not connecting to localhost... send login/pwd */
  {
    if(!FSCA_RequestAuth(Client,Login,Pwd))
    {
      printf("Error sending login/pwd to %s\n",Server);
      return -3;
    }
  }

  if(!global_info && !conn_info)
    conn_info = true;
  if(global_info)
  {
    FSCA_PGlobal Gbl = FSCA_RequestGlobalInfo(Client);
    Plugins = FSCA_Plugin_Enum(Client);
    if(Gbl != NULL)
    {
      printf("Global infos :\n");
      printf("\tName           = %s\n",Gbl->Name);
      printf("\tComment        = %s\n",Gbl->Comment);
      printf("\tMaster         = %s\n",Gbl->Master);
      printf("\tIdle           = %d\n",Gbl->Idle);
      printf("\tMaxConn        = %d\n",Gbl->MaxConn);
      printf("\tMaxXFerPerConn = %d\n",Gbl->MaxXFerPerConn);
      printf("\tFTP            = %d\n",Gbl->FTP);
      printf("\tFTP_MaxConn    = %d\n",Gbl->FTP_MaxConn);
    }
    if(Plugins != NULL)
    {
      printf("\nLoaded plugins :\n");
      while(Plugins != NULL)
      {
        FSCA_PPluginInfo Pl = (FSCA_PPluginInfo) Plugins->Data;
        printf("\t%s %s v%s\n",Pl->Name,Pl->Copyright,Pl->Version);
        Plugins = Plugins->Next;
      }
    }
  }
  if(conn_info)
  {
    if(RequestSharesList(Client) == false)
    {
      printf("Cannot request shares for %s\n",Server);
      return -3;
    }
  }
  if(ejectfrom)
  {
    if(FSCA_RequestEject(Client,ejectfrom))
      printf("Ejected everybody from %s\n",ejectfrom);
    else
      printf("Couldn't ejected from %s (share name correct ?)\n",ejectfrom);
  }

  SU_FreeCS(Client);
  return 0;
}
