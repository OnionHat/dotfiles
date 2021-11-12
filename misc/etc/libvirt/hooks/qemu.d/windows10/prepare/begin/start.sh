#!/bin/sh
set -x

source /etc/libvirt/hooks/kvm.conf

systemctl stop lightdm.service

echo 0 > /sys/class/vtconsole/vtcon0/bind
echo 0 > /sys/class/vtconsole/vtcon1/bind

echo efi-framebuffer.0 > /sys/bus/platform/drivers/efi-framebuffer/unbind

sleep 2

modprobe -r nvidia_drm
modprobe -r nvida
modprobe -r nvidia_modeset
modprobe -r nvidia_uvm
modprobe -r drm
modprobe -r drm_kms_helper

#virsh nodedev-detach --device $VIRSH_GPU_VIDEO
#virsh nodedev-detach --device $VIRSH_GPU_AUDIO

modprobe devname:vfio/vfio

