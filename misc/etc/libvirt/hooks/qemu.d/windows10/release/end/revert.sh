#!/bin/bash
set -x

source /etc/libvirt/hooks/kvm.conf

# Re-Bind GPU to Nvidia Driver
#virsh nodedev-reattach --device $VIRSH_GPU_VIDEO
#virsh nodedev-reattach --device $VIRSH_GPU_AUDIO

# Reload nvidia modules
modprobe nvidia_drm
modprobe nvidia
modprobe nvidia_modeset
modprobe nvidia_uvm
modprobe drm
modprobe drm_kms_helper

##nvidia-xconfig --query-gpu-info > /dev/null 2>&1
echo "efi-framebuffer.0" > /sys/bus/platform/drivers/efi-framebuffer/bind

# Rebind VT consoles
echo 1 > /sys/class/vtconsole/vtcon0/bind
echo 1 > /sys/class/vtconsole/vtcon1/bind

sleep 5
# Restart Display Manager
systemctl start lightdm.service
