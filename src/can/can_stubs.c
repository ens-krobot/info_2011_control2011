/*
 * can_stubs.c
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>

#include <linux/can.h>
#include <string.h>

CAMLprim value ocaml_can_open_can_file_descr(value iface)
{
  /* Create the socket */
  int fd = socket(PF_CAN, SOCK_RAW, CAN_RAW);
  if (fd < 0) uerror("socket", Nothing);

  /* Locate the interface you wish to use */
  struct ifreq ifr;
  strcpy(ifr.ifr_name, String_val(iface));
  /* ifr.ifr_ifindex gets filled with that device's index */
  if (ioctl(fd, SIOCGIFINDEX, &ifr) < 0) uerror("ioctl", Nothing);

  /* Select that CAN interface, and bind the socket to it. */
  struct sockaddr_can addr;
  addr.can_family = AF_CAN;
  addr.can_ifindex = ifr.ifr_ifindex;
  if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) uerror("bind", Nothing);

  return Val_int(fd);
}

CAMLprim value ocaml_can_get_frame_size()
{
  return Val_int(sizeof(struct can_frame));
}

/* Cast a caml string into a can frame. */
#define FRAME(v) ((struct can_frame*)String_val(v))

CAMLprim value ocaml_can_forge_frame(value v)
{
  CAMLparam1(v);
  CAMLlocal2(result, data);
  result = caml_alloc_string(sizeof(struct can_frame));
  FRAME(result)->can_id = Int_val(Field(v, 0)) | (Int_val(Field(v, 1)) >> 29) | (Int_val(Field(v, 2)) >> 30) | (Int_val(Field(v, 3)) >> 31);
  data = Field(v, 4);
  FRAME(result)->can_dlc = caml_string_length(data);
  memcpy(FRAME(result)->data, String_val(data), caml_string_length(data));
  CAMLreturn(result);
}

CAMLprim value ocaml_can_parse_frame(value v)
{
  CAMLparam1(v);
  CAMLlocal2(result, data);
  result = caml_alloc_tuple(5);
  Field(result, 0) = Val_int(FRAME(v)->can_id & CAN_EFF_MASK);
  Field(result, 1) = Val_int((FRAME(v)->can_id << 29) & 1);
  Field(result, 2) = Val_int((FRAME(v)->can_id << 30) & 1);
  Field(result, 3) = Val_int((FRAME(v)->can_id << 31) & 1);
  data = caml_alloc_string(FRAME(v)->can_dlc);
  memcpy(String_val(data), FRAME(v)->data, FRAME(v)->can_dlc);
  Field(result, 4) = data;
  CAMLreturn(result);
}
