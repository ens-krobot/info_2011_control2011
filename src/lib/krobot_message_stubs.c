/*
 * krobot_message_stubs.c
 * ----------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

struct bezier_message {
  unsigned int x : 12;
  unsigned int y : 12;
  int d1 : 9;
  unsigned int d2 : 8;
  int theta : 12;
  unsigned int v : 11;
};

CAMLprim value krobot_message_encode_bezier(value params)
{
  CAMLparam1(params);
  CAMLlocal1(str);
  str = caml_alloc_string(8);
  struct bezier_message *msg = (struct bezier_message*)String_val(str);
  msg->x = Int_val(Field(params, 0));
  msg->y = Int_val(Field(params, 1));
  msg->d1 = Int_val(Field(params, 2));
  msg->d2 = Int_val(Field(params, 3));
  msg->theta = Int_val(Field(params, 4));
  msg->v = Int_val(Field(params, 5));
  CAMLreturn(str);
}

CAMLprim value krobot_message_decode_bezier(value str)
{
  CAMLparam1(str);
  CAMLlocal1(res);
  struct bezier_message *msg = (struct bezier_message*)String_val(str);
  res = caml_alloc_tuple(6);
  Field(res, 0) = Val_int(msg->x);
  Field(res, 1) = Val_int(msg->y);
  Field(res, 2) = Val_int(msg->d1);
  Field(res, 3) = Val_int(msg->d2);
  Field(res, 4) = Val_int(msg->theta);
  Field(res, 5) = Val_int(msg->v);
  CAMLreturn(res);
}
