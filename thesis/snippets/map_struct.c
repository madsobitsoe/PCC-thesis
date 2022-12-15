struct {    /* Used by BPF_MAP_CREATE */
    __u32  map_type;
    __u32  key_size;    /* size of key in bytes */
    __u32  value_size;  /* size of value in bytes */
    __u32  max_entries; /* maximum number of entries in a map */
};
