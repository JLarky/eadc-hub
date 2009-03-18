/*
 * tiger_drv - A Erlang Port_driver to use TIGER hash in Erlang
 * Copyrught (c) 2009, JLarky <jlarky@gmail.com>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "erl_driver.h"
#include <stdio.h>
#include <stdint.h>

extern void tiger(uint64_t* str, uint64_t length, uint64_t* res);

typedef struct {
  ErlDrvPort port;
} tiger_data;

static ErlDrvData tiger_drv_start(ErlDrvPort port, char *buff)
{
  tiger_data* d = (tiger_data*)driver_alloc(sizeof(tiger_data));
  d->port = port;
  return (ErlDrvData)d;
}

static void tiger_drv_stop(ErlDrvData handle)
{
  driver_free((char*)handle);
}

static void tiger_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
  tiger_data* d = (tiger_data*)handle;
  
  uint64_t tiger_res[3];

  tiger((uint64_t*) buff, bufflen, (uint64_t*) tiger_res);

  driver_output(d->port, tiger_res, 24);
}

ErlDrvEntry tiger_driver_entry = {
  NULL,                       /* F_PTR init, N/A */
  tiger_drv_start,            /* L_PTR start, called when port is opened */
  tiger_drv_stop,             /* F_PTR stop, called when port is closed */
  tiger_drv_output,           /* F_PTR output, called when erlang has sent */
  NULL,                       /* F_PTR ready_input, called when input descriptor ready */
  NULL,                       /* F_PTR ready_output, called when output descriptor ready */
  "tiger_drv",                /* char *driver_name, the argument to open_port */
  NULL,                       /* F_PTR finish, called when unloaded */
  NULL,                       /* F_PTR control, port_command callback */
  NULL,                       /* F_PTR timeout, reserved */
  NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(tiger) /* must match name in driver_entry */
{
  return &tiger_driver_entry;
}
