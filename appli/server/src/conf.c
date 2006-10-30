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
#include "server.h"

int FS_GetIntLen(int v)
{
  if(v < 10)
    return 1;
  if(v < 100)
    return 2;
  if(v < 1000)
    return 3;
  if(v < 10000)
    return 4;
  return 5;
}

SU_THREAD_ROUTINE(FS_ClientConf,Info)
{
  SU_PClientSocket Client = (SU_PClientSocket) Info;
  FFSS_Field Size,pos,User;
  char *buf;
  unsigned int buf_len;
  char *s_p,*s_n,*s_c,*s_w,*s_pr,*s_nchk,*s_m,*s_u;
  char *g_n,*g_c,*g_m,*g_i,*g_max,*g_max_xf,*g_f,*g_f_max,*g_xf_conn;
  char *q,*r;
  char *u_l,*u_p,*u_w;
  FS_PShare Share,shr2;
  FS_PConn Conn;
  FS_PUser Usr;
  FS_PPlugin Plugin;
  int res,nb,nb2,retval,c1,c2;
  bool error;
  SU_PList Ptr,Ptr2;
  fd_set rfds;
  struct timeval tv;
  char *Users;

  SU_ThreadBlockSigs();
  buf_len = 10000;
  buf = (char *) malloc(buf_len);
  Users = (char *) malloc(buf_len);
  while(1)
  {
    pos = 0;
    /* Receiving length of the command */
    res = recv(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
    if(res == SOCKET_ERROR)
    {
      SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration socket disconnected (read error)");
      SU_FreeCS(Client);
      free(buf);
      free(Users);
      SU_END_THREAD(NULL);
    }
    else if(res == 0)
    {
      SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration socket disconnected (null paquet)");
      SU_FreeCS(Client);
      free(buf);
      free(Users);
      SU_END_THREAD(NULL);
    }

    if(Size >= buf_len)
    {
      SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration socket disconnected (Command greater than receive buffer)");
      SU_FreeCS(Client);
      free(buf);
      free(Users);
      SU_END_THREAD(NULL);
    }
    while(pos < Size)
    {
      /* Receiving the command */
      FD_ZERO(&rfds);
      FD_SET(Client->sock,&rfds);
      tv.tv_sec = 5;
      tv.tv_usec = 0;
      retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
      if(!retval)
      {
        SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration socket disconnected (timed out)");
        SU_FreeCS(Client);
        free(buf);
        free(Users);
        SU_END_THREAD(NULL);
      }
      res = recv(Client->sock,buf+pos,Size-pos,SU_MSG_NOSIGNAL);
      if(res == SOCKET_ERROR)
      {
        SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration socket disconnected (read error)");
        SU_FreeCS(Client);
        free(buf);
        free(Users);
        SU_END_THREAD(NULL);
      }
      else if(res == 0)
      {
        SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration socket disconnected (null paquet)");
        SU_FreeCS(Client);
        free(buf);
        free(Users);
        SU_END_THREAD(NULL);
      }
      pos += res;
    }
    pos = 0; /* We discard any other command from the buffer */
    SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Command found... analysing");
    /* Analyse command */
    error = false;
    switch(buf[0]) // Command op-code
    {
      case FS_OPCODE_ADDSHARE :
        pos = 1;
        s_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if(s_n == NULL)
        { /* Attack */
          error = true;
          break;
        }
        if(s_n[0] == 0)
        { /* Empty name */
          SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Empty name... rejecting");
          Size = 1;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
          break;
        }
        SU_SEM_WAIT(FS_SemShr);
        if(FS_GetShareFromName(s_n) != NULL) /* If a share with this name already exists */
        {
          SU_SEM_POST(FS_SemShr);
          SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Share with same name already exists : %s",s_n);
          Size = 1;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
          break;
        }
        SU_SEM_POST(FS_SemShr);
        s_p = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_c = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_w = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_pr = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_nchk = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_m = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_u = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if((s_p == NULL) || (s_c == NULL) || (s_w == NULL) || (s_pr == NULL) || (s_nchk == NULL) || (s_m == NULL) || (s_u == NULL))
        { /* Attack */
          error = true;
          break;
        }
        /* Parse users string */
        Ptr = NULL;
        q = s_u;
        r = strchr(q,',');
        while(r != NULL)
        {
          r[0] = 0; r++;
          u_l = q;
          q = r;
          r = strchr(q,',');
          if(r == NULL)
            break;
          r[0] = 0; r++;
          u_p = q;
          q = r;
          r = strchr(q,',');
          if(r != NULL)
          {
            r[0] = 0; r++;
          }
          u_w = q;
          q = r;
          Usr = (FS_PUser) malloc(sizeof(FS_TUser));
          memset(Usr,0,sizeof(FS_TUser));
          Usr->Login = strdup(u_l);
          Usr->Password = strdup(u_p);
          Usr->Writeable = atoi(u_w);
          Ptr = SU_AddElementHead(Ptr,Usr);
          if(q == NULL)
            break;
          else
            r = strchr(q,',');
        }
        /* Building index */
        FS_BuildIndex(s_p,s_n,s_c,(bool)atoi(s_w),(bool)atoi(s_pr),(bool)atoi(s_nchk),atoi(s_m),Ptr,true);
#ifdef _WIN32
        FS_SaveConfig(NULL);
#endif /* _WIN32 */
        Size = 1;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        buf[0] = FS_OPCODE_ACK;
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_DELSHARE :
        pos = 1;
        s_p = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromPath(s_p);
        if(Share == NULL)
        {
          SU_SEM_POST(FS_SemShr);
          SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Share with this path does not exist : %s",s_p);
          Size = 1;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
          break;
        }
        FS_Index = SU_DelElementElem(FS_Index,Share);
        FS_FreeShare(Share);
        SU_SEM_POST(FS_SemShr);
#ifdef _WIN32
        FS_SaveConfig(NULL);
#endif /* _WIN32 */
        Size = 1;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        buf[0] = FS_OPCODE_ACK;
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_GETGLOBAL :
        buf[0] = FS_OPCODE_ACK;
        pos = 1;
        SU_SEM_WAIT(FS_SemGbl);
        SU_strcpy(buf+pos,FS_MyGlobal.Name,buf_len-pos);
        pos += strlen(FS_MyGlobal.Name) + 1;
        SU_strcpy(buf+pos,FS_MyGlobal.Comment,buf_len-pos);
        pos += strlen(FS_MyGlobal.Comment) + 1;
        if(FS_MyGlobal.Master == NULL)
        {
          SU_strcpy(buf+pos,"",buf_len-pos);
          pos += strlen("") + 1;
        }
        else
        {
          SU_strcpy(buf+pos,FS_MyGlobal.Master,buf_len-pos);
          pos += strlen(FS_MyGlobal.Master) + 1;
        }
        snprintf(buf+pos,buf_len-pos,"%d%c%d%c%d%c%d%c%d%c%d",FS_MyGlobal.Idle,0,FS_MyGlobal.MaxConn,0,FS_MyGlobal.MaxXFerPerConn,0,FS_MyGlobal.FTP,0,FS_MyGlobal.FTPMaxConn,0,FS_MyGlobal.XFerInConn);
        pos += FS_GetIntLen(FS_MyGlobal.Idle) + FS_GetIntLen(FS_MyGlobal.MaxConn) + FS_GetIntLen(FS_MyGlobal.MaxXFerPerConn) + FS_GetIntLen(FS_MyGlobal.FTP) + FS_GetIntLen(FS_MyGlobal.FTPMaxConn) + FS_GetIntLen(FS_MyGlobal.XFerInConn) + 6;
        SU_SEM_POST(FS_SemGbl);
        Size = pos;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_GETSHARE :
        pos = 1;
        s_p = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if(s_p == NULL)
        {
          error = true;
          break;
        }
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromPath(s_p);
        if(Share == NULL)
        {
          SU_SEM_POST(FS_SemShr);
          Size = 1;
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
          break;
        }
        buf[0] = FS_OPCODE_ACK;
        pos = 1;
        SU_strcpy(buf+pos,Share->ShareName,buf_len-pos);
        pos += strlen(Share->ShareName) + 1;
        SU_strcpy(buf+pos,Share->Comment,buf_len-pos);
        pos += strlen(Share->Comment) + 1;
        /* Build users string */
        Ptr = Share->Users;
        Users[0] = 0;
        while(Ptr != NULL)
        {
          Usr = (FS_PUser) Ptr->Data;
          SU_strcat(Users,Usr->Login,buf_len);
          SU_strcat(Users,",",buf_len);
          SU_strcat(Users,Usr->Password,buf_len);
          if(Usr->Writeable)
            SU_strcat(Users,",1",buf_len);
          else
            SU_strcat(Users,",0",buf_len);
          if(Ptr->Next != NULL)
            SU_strcat(Users,",",buf_len);
          Ptr = Ptr->Next;
        }
        snprintf(buf+pos,buf_len-pos,"%d%c%d%c%d%c%d%c%s",Share->Writeable,0,Share->Private,0,Share->NoChksum,0,Share->MaxConnections,0,Users);
        pos += FS_GetIntLen(Share->Writeable) + FS_GetIntLen(Share->Private) + FS_GetIntLen(Share->NoChksum) + FS_GetIntLen(Share->MaxConnections) + strlen(Users) + 4;
        Size = pos;
        SU_SEM_POST(FS_SemShr);
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_GETNAMEAVAIL :
        pos = 1;
        s_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if(s_n == NULL)
        {
          error = true;
          break;
        }
        Size = 1;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        SU_SEM_WAIT(FS_SemShr);
        if(FS_GetShareFromName(s_n) != NULL)
          buf[0] = FS_OPCODE_NACK;
        else
          buf[0] = FS_OPCODE_ACK;
        SU_SEM_POST(FS_SemShr);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_UPDTSHARE :
        pos = 1;
        s_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_p = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if((s_n == NULL) || (s_p == NULL))
        {
          error = true;
          break;
        }
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromPath(s_p);
        shr2 = FS_GetShareFromName(s_n);
        if((Share == NULL) || ((shr2 != NULL) && (shr2 != Share)))
        {
          SU_SEM_POST(FS_SemShr);
          SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Share with this path does not exist, or with this name exists : %s -> %s",s_p,s_n);
          Size = 1;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
          break;
        }
        s_c = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_w = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_pr = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_nchk = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_m = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_u = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if((s_c == NULL) || (s_w == NULL) || (s_pr == NULL) || (s_nchk == NULL) || (s_m == NULL) || (s_u == NULL))
        {
          SU_SEM_POST(FS_SemShr);
          error = true;
          break;
        }
        FS_EjectFromShare(Share,false);
        free(Share->ShareName);
        Share->ShareName = strdup(s_n);
        if(Share->Comment != NULL)
          free(Share->Comment);
        Share->Comment = strdup(s_c);
        Share->Writeable = (bool)atoi(s_w);
        Share->Private = (bool)atoi(s_pr);
        Share->NoChksum = (bool)atoi(s_nchk);
        Share->MaxConnections = atoi(s_m);
        Ptr = Share->Users;
        while(Ptr != NULL)
        {
          FS_FreeUser((FS_PUser)Ptr->Data);
          Ptr = Ptr->Next;
        }
        /* Parse users string */
        Ptr = NULL;
        q = s_u;
        r = strchr(q,',');
        while(r != NULL)
        {
          r[0] = 0; r++;
          u_l = q;
          q = r;
          r = strchr(q,',');
          if(r == NULL)
            break;
          r[0] = 0; r++;
          u_p = q;
          q = r;
          r = strchr(q,',');
          if(r != NULL)
          {
            r[0] = 0; r++;
          }
          u_w = q;
          q = r;
          Usr = (FS_PUser) malloc(sizeof(FS_TUser));
          memset(Usr,0,sizeof(FS_TUser));
          Usr->Login = strdup(u_l);
          Usr->Password = strdup(u_p);
          Usr->Writeable = atoi(u_w);
          Ptr = SU_AddElementHead(Ptr,Usr);
          if(q == NULL)
            break;
          else
            r = strchr(q,',');
        }
        Share->Users = Ptr;
        SU_SEM_POST(FS_SemShr);
#ifdef _WIN32
        FS_SaveConfig(NULL);
#endif /* _WIN32 */
        Size = 1;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        buf[0] = FS_OPCODE_ACK;
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_UPDTGLOBAL :
      {
        bool MasterChanged = true;
        pos = 1;
        g_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_c = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_m = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_i = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_max = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_max_xf = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_f = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_f_max = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        g_xf_conn = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if((g_n == NULL) || (g_c == NULL) || (g_m == NULL) || (g_i == NULL) || (g_max == NULL) || (g_f == NULL) || (g_f_max == NULL) || (g_max_xf == NULL) || (g_xf_conn == NULL))
        {
          error = true;
          break;
        }
        SU_SEM_WAIT(FS_SemGbl);
        free(FS_MyGlobal.Name);
        FS_MyGlobal.Name = strdup(g_n);
        if(FS_MyGlobal.Comment != NULL)
          free(FS_MyGlobal.Comment);
        FS_MyGlobal.Comment = strdup(g_c);
        if(FS_MyGlobal.Master != NULL)
        {
          MasterChanged = strcmp(FS_MyGlobal.Master,g_m) != 0;
          free(FS_MyGlobal.Master);
          free(FS_MyGlobal.MasterIP);
        }
        if(g_m[0] == 0) /* No master */
          FS_MyGlobal.Master = NULL;
        else
          FS_MyGlobal.Master = strdup(g_m);
        FS_MyGlobal.Idle = atoi(g_i);
        FS_MyGlobal.MaxConn = atoi(g_max);
        FS_MyGlobal.MaxXFerPerConn = atoi(g_max_xf);
        FS_MyGlobal.FTP = (bool)atoi(g_f);
        FS_MyGlobal.FTPMaxConn = (bool)atoi(g_f_max);
        FS_MyGlobal.XFerInConn = (bool)atoi(g_xf_conn);
        if(FS_CheckGlobal() != NULL)
        {
          SU_SEM_POST(FS_SemGbl);
          SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Cannot restart server.. error in Global fields");
          Size = 1;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
          break;
        }
        SU_SEM_POST(FS_SemGbl);
        FS_SaveConfig(CONFIG_FILE_NAME);
        Size = 1;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        buf[0] = FS_OPCODE_ACK;
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        if(MasterChanged)
        {
          SU_SEM_WAIT(FS_SemGbl);
          /* Sending login message to my master */
          FS_SendMessage_State(FS_MyGlobal.Master,FS_MyGlobal.Name,FFSS_GetOS(),FS_MyGlobal.Comment,FFSS_STATE_ON);
          /* Sending index message to my master */
          FS_SendIndex(FS_MyGlobal.Master,FFSS_MASTER_PORT_S);
          SU_SEM_POST(FS_SemGbl);
        }
        break;
      }
      case FS_OPCODE_SETSTATE :
        FS_MyState = buf[1];
        SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Changing state of the server : %d",FS_MyState);
        SU_SEM_WAIT(FS_SemGbl);
        if(FS_MyGlobal.Master != NULL)
        {
          /* Sending state message to my master */
          FS_SendMessage_State(FS_MyGlobal.Master,FS_MyGlobal.Name,FFSS_GetOS(),FS_MyGlobal.Comment,FS_MyState);
        }
        SU_SEM_POST(FS_SemGbl);
        break;
      case FS_OPCODE_GETSTATE :
        Size = 2;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        buf[0] = FS_OPCODE_ACK;
        buf[1] = FS_MyState;
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_GETSHRLIST :
        buf[0] = FS_OPCODE_ACK;
        pos = 1;
        SU_SEM_WAIT(FS_SemShr);
        snprintf(buf+pos,buf_len-pos,"%d",SU_ListCount(FS_Index));
        pos += FS_GetIntLen(SU_ListCount(FS_Index)) + 1;
        Ptr = FS_Index;
        while(Ptr != NULL)
        {
          Share = (FS_PShare)Ptr->Data;
          SU_strcpy(buf+pos,Share->ShareName,buf_len-pos);
          pos += strlen(Share->ShareName) + 1;
          SU_strcpy(buf+pos,Share->Path,buf_len-pos);
          pos += strlen(Share->Path) + 1;
          Ptr2 = Share->Conns;
          nb = 0;
          nb2 = 0;
          while(Ptr2 != NULL)
          {
            Conn = (FS_PConn)Ptr2->Data;
            nb2 += SU_ListCount(Conn->XFers) + SU_ListCount(Conn->Strms);
            nb++;
            Ptr2 = Ptr2->Next;
          }
          snprintf(buf+pos,buf_len-pos,"%d%c%d%c%d",Share->Disabled,0,nb,0,nb2);
          pos += FS_GetIntLen(Share->Disabled) + FS_GetIntLen(nb) + FS_GetIntLen(nb2) + 3;
          Ptr = Ptr->Next;
        }
        SU_SEM_POST(FS_SemShr);
        Size = pos;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_RESCAN :
        pos = 1;
        s_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if(s_n == NULL)
        {
          error = true;
          break;
        }
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromName(s_n);
        if(Share == NULL)
        {
          error = true;
          SU_SEM_POST(FS_SemShr);
          break;
        }
        FS_EjectFromShare(Share,true);
        FS_RescanShare(Share);
        SU_SEM_POST(FS_SemShr);
        break;
      case FS_OPCODE_SETSHARESTATE:
        pos = 2;
        s_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if(s_n == NULL)
        {
          error = true;
          break;
        }
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromName(s_n);
        if(Share != NULL)
          Share->Disabled = (buf[1] == 0);
        SU_SEM_POST(FS_SemShr);
        break;
      case FS_OPCODE_EJECT :
        pos = 1;
        s_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        if(s_n == NULL)
        {
          error = true;
          break;
        }
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromName(s_n);
        if(Share != NULL)
          FS_EjectFromShare(Share,true);
        SU_SEM_POST(FS_SemShr);
        break;
      case FS_OPCODE_GETSHRCONNS :
        pos = 1;
        s_p = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromPath(s_p);
        if(Share == NULL)
        {
          SU_SEM_POST(FS_SemShr);
          SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration : Share with this path does not exist : %s",s_p);
          Size = 1;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
          break;
        }
        buf[0] = FS_OPCODE_ACK;
        pos = 1;
        snprintf(buf+pos,buf_len-pos,"%d",SU_ListCount(Share->Conns));
        pos += FS_GetIntLen(SU_ListCount(Share->Conns)) + 1;
        Ptr = Share->Conns;
        while(Ptr != NULL)
        {
          Conn = (FS_PConn)Ptr->Data;
          SU_strcpy(buf+pos,Conn->Remote,buf_len-pos);
          pos += strlen(Conn->Remote) + 1;
          c1 = SU_ListCount(Conn->XFers);
          c2 = SU_ListCount(Conn->Strms);
          snprintf(buf+pos,buf_len-pos,"%d%c%d",c1,0,c2);
          pos += FS_GetIntLen(c1) + FS_GetIntLen(c2) + 2;
          Ptr2 = Conn->XFers;
          while(Ptr2 != NULL)
          {
            c1 = (int)((float)((FFSS_PTransfer)Ptr2->Data)->XFerPos / (float)((FFSS_PTransfer)Ptr2->Data)->FileSize * 100.0);
            snprintf(buf+pos,buf_len-pos,"%s%c%d",((FFSS_PTransfer)Ptr2->Data)->LocalPath,0,c1);
            pos += strlen(((FFSS_PTransfer)Ptr2->Data)->LocalPath) + FS_GetIntLen(c1) + 2;
            Ptr2 = Ptr2->Next;
          }
          Ptr2 = Conn->Strms;
          while(Ptr2 != NULL)
          {
            c1 = (int)((float)((FS_PStreaming)Ptr2->Data)->Position / (float)((FS_PStreaming)Ptr2->Data)->fsize * 100.0);
            snprintf(buf+pos,buf_len-pos,"%s%c%d",((FS_PStreaming)Ptr2->Data)->FileName,0,c1);
            pos += strlen(((FS_PStreaming)Ptr2->Data)->FileName) + FS_GetIntLen(c1) + 2;
            Ptr2 = Ptr2->Next;
          }
          Ptr = Ptr->Next;
        }
        SU_SEM_POST(FS_SemShr);
        Size = pos;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_EJECTIP :
        pos = 1;
        s_p = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        s_n = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        SU_SEM_WAIT(FS_SemShr);
        Share = FS_GetShareFromPath(s_p);
        if((Share != NULL) && (s_n != NULL))
          FS_EjectFromShareByIP(Share,s_n,true);
        SU_SEM_POST(FS_SemShr);
        break;
      case FS_OPCODE_PL_LOAD :
        pos = 1;
        s_p = FFSS_UnpackString(buf,buf+pos,Size,&pos);
        Plugin = FS_LoadPlugin(s_p);
        if(Plugin == NULL)
        {
          Size = 1;
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          buf[0] = FS_OPCODE_NACK;
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        }
        else
        {
          if(buf[pos]) /* Add plugin at server startup */
          {
            FS_AddPluginToStartup(Plugin);
          }
          Size = 1;
          buf[0] = FS_OPCODE_ACK;
          Size = FFSS_PackField(buf,Size,(FFSS_Field)Plugin);
          send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
          send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        }
        break;
      case FS_OPCODE_PL_UNLOAD :
        pos = 1;
        Plugin = (FS_PPlugin) FFSS_UnpackField(buf,buf+pos,Size,&pos);
        Size = 1;
        if(FS_IsPluginValid(Plugin))
        {
          if(buf[pos]) /* Remove plugin from server startup */
          {
            FS_RemovePluginFromStartup(Plugin);
          }
          FS_UnLoadPlugin(Plugin->Handle);
          buf[0] = FS_OPCODE_ACK;
        }
        else
          buf[0] = FS_OPCODE_NACK;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_PL_CONFIGURE :
        pos = 1;
        Plugin = (FS_PPlugin) FFSS_UnpackField(buf,buf+pos,Size,&pos);
        User = FFSS_UnpackField(buf,buf+pos,Size,&pos);
        Size = 1;
        if(FS_IsPluginValid(Plugin))
        {
          if(FS_ConfigurePlugin(Plugin->Handle,(void *)User))
            buf[0] = FS_OPCODE_ACK;
          else
            buf[0] = FS_OPCODE_NACK;
        }
        else
          buf[0] = FS_OPCODE_NACK;
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      case FS_OPCODE_PL_ENUM :
        Size = 1;
        buf[0] = FS_OPCODE_ACK;
        SU_SEM_WAIT(FS_SemPlugin);
        Size = FFSS_PackField(buf,Size,SU_ListCount(FS_Plugins));
        Ptr = FS_Plugins;
        while(Ptr != NULL)
        {
          Plugin = (FS_PPlugin) Ptr->Data;
          Size = FFSS_PackField(buf,Size,(FFSS_Field)Plugin);
          Size = FFSS_PackString(buf,Size,Plugin->Name,strlen(Plugin->Name)+1);
          Size = FFSS_PackString(buf,Size,Plugin->Copyright,strlen(Plugin->Copyright)+1);
          Size = FFSS_PackString(buf,Size,Plugin->Version,strlen(Plugin->Version)+1);
          buf[Size++] = (char) Plugin->Startup;
          buf[Size++] = (char) Plugin->Configurable;
          Ptr = Ptr->Next;
        }
        SU_SEM_POST(FS_SemPlugin);
        send(Client->sock,(char *)&Size,sizeof(Size),SU_MSG_NOSIGNAL);
        send(Client->sock,buf,Size,SU_MSG_NOSIGNAL);
        break;
      default :
        SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client from runtime configuration socket disconnected (unknown opcode : %d)",buf[0]);
        error = true;
    }
    if(error)
    {
      SU_FreeCS(Client);
      free(buf);
      free(Users);
      SU_END_THREAD(NULL);
    }
  }
}

bool FS_CheckConfConn(SU_PClientSocket Client)
{
  SU_PList Ptr;
  bool ret_val = true,checked = false;

  SU_SEM_WAIT(FS_SemPlugin);
  Ptr = FS_Plugins;
  while(Ptr != NULL)
  {
    if(((FS_PPlugin)Ptr->Data)->OnCheckConfConn != NULL)
    {
      ret_val &= ((FS_PPlugin)Ptr->Data)->OnCheckConfConn(Client);
      checked = true;
    }
    Ptr = Ptr->Next;
  }
  SU_SEM_POST(FS_SemPlugin);
  if(!ret_val) /* Rejected by plugin */
    return false;
  if(checked) /* Accepted by plugin */
    return true;

  if(strcmp(inet_ntoa(Client->SAddr.sin_addr),"127.0.0.1") != 0)
  { /* Non local socket... rejecting */
    SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"WARNING : Non local socket for runtime configuration... DoS Attack ?");
    return false;
  }
  return true;
}

SU_THREAD_ROUTINE(FS_ConfFunc,Info)
{
  SU_PServerInfo SI = (SU_PServerInfo) Info;
  SU_PClientSocket Client;
  SU_THREAD_HANDLE ClientThr;

  SU_ThreadBlockSigs();
  while(FS_MyState == FFSS_STATE_OFF) /* Wait till server is up */
  {
    SU_SLEEP(1);
  }
  if(SU_ServerListen(SI) == SOCKET_ERROR)
  {
    FFSS_PrintSyslog(LOG_WARNING,"Cannot create listening socket for runtime configuration (listen)\n");
    SU_FreeSI(SI);
    SU_END_THREAD(NULL);
  }
  while(1)
  {
    Client = SU_ServerAcceptConnection(SI);
    if(Client == NULL)
    {
      /* Server may being shut down */
      SU_SLEEP(1);
      continue;
    }
    SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Client connected on runtime configuration port of the server from %s (%s)",inet_ntoa(Client->SAddr.sin_addr),SU_NameOfPort(inet_ntoa(Client->SAddr.sin_addr)));
    if(!FS_CheckConfConn(Client))
    { /* rejecting */
      SU_FreeCS(Client);
      continue;
    }
    SU_DBG_PrintDebug(FS_DBGMSG_CONF_CONN,"Connection accepted ... creating new thread");
    if(!SU_CreateThread(&ClientThr,FS_ClientConf,(void *)Client,true))
    {
      SU_DBG_PrintDebug(FFSS_DBGMSG_WARNING,"Error creating conf Client thread");
      SU_FreeCS(Client);
      continue;
    }
  }
  SU_END_THREAD(NULL);
}

