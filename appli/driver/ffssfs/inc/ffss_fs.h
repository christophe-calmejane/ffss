/*
    This is a ffssfs file system driver for Windows NT/2000/XP.
    Copyright (C) 2002 Christophe Calmejane
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

#ifndef _ROM_FS_
#define _ROM_FS_

#define FFSSDISK_ID	 "FFSS Disk 1.0"
#define DBGNAME	     "FFSS FS : "
#define FFSSLABEL    "FFSS Net"

#define FFSS_DEFINES_ONLY
#define SU_PClientSocket void *
#include <ffss.h>

//#define SECTOR_SIZE         512

//#define BLOCK_SIZE          PAGE_SIZE

//#define ROMFS_MAGIC         "-rom1fs-"

//#define ROMFS_MAGIC_OFFSET  0

/*#define SIZEOF_FFSS_INODE(x) \
    ((sizeof(struct ffss_inode) + ROMFH_PAD) \
    & ROMFH_MASK)*/

//
// Types used by Linux
//
#include "ltypes.h"

//
// Use 1 byte packing of on-disk structures
//
#include <pshpack1.h>

//
// The following is a subset of linux/include/linux/romfs_fs.h from
// version 2.2.14
//

/* The basic structures of the romfs filesystem */

/*#define ROMBSIZE    BLOCK_SIZE

#define ROMFS_MAXFN 128

#define ROMFH_TYPE  7
#define ROMFH_HRD   0
#define ROMFH_DIR   1
#define ROMFH_REG   2
#define ROMFH_SYM   3
#define ROMFH_BLK   4
#define ROMFH_CHR   5
#define ROMFH_SCK   6
#define ROMFH_FIF   7
#define ROMFH_EXEC  8
*/
/* Alignment */

/*#define ROMFH_SIZE  16
#define ROMFH_PAD   (ROMFH_SIZE-1)
#define ROMFH_MASK  (~ROMFH_PAD)*/


//
// End of subset of linux/include/linux/romfs_fs.h
//

//
// Use default packing of structures
//
#include <poppack.h>

#endif
