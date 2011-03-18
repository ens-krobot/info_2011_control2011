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
#include <caml/fail.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>

#include <linux/can.h>
#include <string.h>
#include <stdint.h>

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

CAMLprim value ocaml_can_recv(value val_fd)
{
  CAMLparam1(val_fd);
  CAMLlocal3(val_result, val_frame, val);

  struct msghdr msg;
  struct iovec iov;
  uint64_t timestamp;
  struct can_frame frame;

  /* Prepare the IO vector. */
  iov.iov_base = &frame;
  iov.iov_len = sizeof(frame);

  /* Prepare the message. */
  memset(&msg, 0, sizeof(msg));
  msg.msg_iov = &iov;
  msg.msg_iovlen = 1;
  msg.msg_control = &timestamp;
  msg.msg_controllen = sizeof(timestamp);

  /* Receive one frame. */
  int ret = recvmsg(Int_val(val_fd), &msg, 0);
  if (ret < 0) uerror("recvmsg", Nothing);

  /* It is an error if we do not receive exactly one frame. */
  if (ret != sizeof(frame)) caml_failwith("recvmsg: invalid size");

  /* Build the caml frame. */
  val_frame = caml_alloc_tuple(5);
  Field(val_frame, 0) = Val_int(frame.can_id & CAN_EFF_MASK);
  Field(val_frame, 1) = Val_int((frame.can_id << 29) & 1);
  Field(val_frame, 2) = Val_int((frame.can_id << 30) & 1);
  Field(val_frame, 3) = Val_int((frame.can_id << 31) & 1);
  val = caml_alloc_string(frame.can_dlc);
  memcpy(String_val(val), frame.data, frame.can_dlc);
  Field(val_frame, 4) = val;

  /* Build the result containing the timestamp and the frame. */
  val_result = caml_alloc_tuple(2);
  val = caml_copy_double(timestamp * 1e-9);
  Store_field(val_result, 0, val);
  Store_field(val_result, 1, val_frame);

  CAMLreturn(val_result);
}

CAMLprim value ocaml_can_send(value val_fd, value val_arg)
{
  struct msghdr msg;
  struct iovec iov;
  uint64_t timestamp;
  struct can_frame frame;

  timestamp = (uint64_t)(Double_val(Field(val_arg, 0)) * 1e9);

  /* Build the can frame. */
  value val_frame = Field(val_arg, 1);
  frame.can_id = Int_val(Field(val_frame, 0)) |
    (Int_val(Field(val_frame, 1)) >> 29) |
    (Int_val(Field(val_frame, 2)) >> 30) |
    (Int_val(Field(val_frame, 3)) >> 31);
  value val_data = Field(val_frame, 4);
  frame.can_dlc = caml_string_length(val_data);
  memcpy(frame.data, String_val(val_data), caml_string_length(val_data));

  /* Prepare the IO vector. */
  iov.iov_base = &frame;
  iov.iov_len = sizeof(frame);

  /* Prepare the message. */
  memset(&msg, 0, sizeof(msg));
  msg.msg_iov = &iov;
  msg.msg_iovlen = 1;
  msg.msg_control = &timestamp;
  msg.msg_controllen = sizeof(timestamp);

  /* Receive one frame. */
  int ret = sendmsg(Int_val(val_fd), &msg, 0);
  if (ret < 0) uerror("sendmsg", Nothing);

  /* It is an error if we do not sent exactly one frame. */
  if (ret != sizeof(frame)) caml_failwith("sendmsg: invalid size");

  return Val_unit;
}
