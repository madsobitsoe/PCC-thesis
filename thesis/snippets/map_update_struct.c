int bpf_update_elem(int fd, const void *key, const void *value, uint64_t flags) {
    union bpf_attr attr = {
	.map_fd = fd,
	.key    = ptr_to_u64(key),
	.value  = ptr_to_u64(value),
	.flags  = flags,
    };
    return bpf(BPF_MAP_UPDATE_ELEM, &attr, sizeof(attr));
}
