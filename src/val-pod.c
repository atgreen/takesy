/* val-pod.c -- validate a hand-built SPA POD by parsing it with real SPA.
 *
 *   cc $(pkg-config --cflags libpipewire-0.3) -o val-pod val-pod.c
 *   ./val-pod /path/to/pod.bin
 *
 * Reads the POD bytes produced by the Lisp builder and prints them with
 * spa_debug_pod(); if the tree matches the intended EnumFormat, our byte
 * layout is correct. Also sanity-checks that the top-level object parses.
 */
#include <stdio.h>
#include <stdlib.h>
#include <spa/pod/pod.h>
#include <spa/pod/iter.h>
#include <spa/debug/pod.h>
#include <spa/param/format.h>

int main(int argc, char **argv)
{
    if (argc < 2) { fprintf(stderr, "usage: %s pod.bin\n", argv[0]); return 2; }
    FILE *f = fopen(argv[1], "rb");
    if (!f) { perror("fopen"); return 2; }
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    void *buf = malloc(n);
    if (fread(buf, 1, n, f) != (size_t)n) { perror("fread"); return 2; }
    fclose(f);

    struct spa_pod *pod = (struct spa_pod *)buf;
    printf("file bytes: %ld\n", n);
    printf("pod: size=%u type=%u (total=%zu)\n",
           SPA_POD_SIZE(pod), SPA_POD_TYPE(pod), SPA_POD_SIZE(pod) + 8);

    if (!spa_pod_is_object(pod)) {
        printf("NOT a POD object -- layout is wrong\n");
        return 1;
    }
    printf("object_type=%u object_id=%u\n",
           SPA_POD_OBJECT_TYPE(pod), SPA_POD_OBJECT_ID(pod));

    int nprops = 0;
    struct spa_pod_prop *p;
    SPA_POD_OBJECT_FOREACH((struct spa_pod_object *)pod, p) {
        printf("  prop key=%u  value type=%u size=%u\n",
               p->key, SPA_POD_TYPE(&p->value), SPA_POD_SIZE(&p->value));
        nprops++;
    }
    printf("props parsed: %d\n", nprops);

    printf("---- spa_debug_pod ----\n");
    spa_debug_pod(0, NULL, pod);
    return 0;
}
