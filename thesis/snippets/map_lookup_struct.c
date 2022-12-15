int bpf_lookup_elem(int fd, const void *key, void *value) {
    union bpf_attr attr = {
	.map_fd = fd,
	.key    = ptr_to_u64(key),
	.value  = ptr_to_u64(value),
    };
    return bpf(BPF_MAP_LOOKUP_ELEM, &attr, sizeof(attr));
}
