#include <stdio.h>

#include "io-server/circular_buffer.h"

const int NUM_ELEMENTS = 200;
const int NUM_CHARS = 60 * sizeof(data_element);

int main()
{
    circular_buffer_p cb = CB_create(NUM_ELEMENTS);

    if (cb == NULL) 
    {
        printf("Couldn't even create the buffer...\n");
        return -1;
    }

    {
        int i = 1;
        CB_atomic_put(cb, &i, 1, CB_COMMIT);

        int result;
        CB_atomic_get(cb, &result, 1, CB_COMMIT);

        printf("result = %d\n", result);
        if (result != 1)
        {
            printf("wrong result!\n");
            return -1;
        }
    }

    {
        char base_message[NUM_CHARS + 6];
        char* message = base_message + 2;
        char received_message[NUM_CHARS + 6];
        for (int i = 0; i < NUM_CHARS + 1; ++i)
            base_message[i] = 'a' + i % 26;
        base_message[NUM_CHARS + 1] = '\0';

        printf("message:  %s\n", message);

        int result;

        result = CB_atomic_put(cb, message, NUM_CHARS / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        result = CB_atomic_get(cb, &received_message, NUM_CHARS / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        printf("received: %s (len = %d)\n", received_message, strlen(received_message));
        if (strlen(received_message) != NUM_CHARS - 1)
        {
            printf("Received wrong length! Got %d, should be %d\n", strlen(received_message), NUM_CHARS - 1);
            exit(-1);
        }

        result = CB_atomic_put(cb, message + 1, (NUM_CHARS - 1) / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        result = CB_atomic_get(cb, &received_message, (NUM_CHARS - 1) / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        printf("received: %s (len = %d)\n", received_message, strlen(received_message));
        if (strlen(received_message) != NUM_CHARS - 2)
        {
            printf("Received wrong length! Got %d, should be %d\n", strlen(received_message), NUM_CHARS - 2);
            exit(-1);
        }

        result = CB_atomic_put(cb, message + 2, (NUM_CHARS - 2) / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        result = CB_atomic_get(cb, &received_message, (NUM_CHARS - 2) / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        printf("received: %s (len = %d)\n", received_message, strlen(received_message));
        if (strlen(received_message) != NUM_CHARS - 3)
        {
            printf("Received wrong length! Got %d, should be %d\n", strlen(received_message), NUM_CHARS - 3);
            exit(-1);
        }

        result = CB_atomic_put(cb, message + 3, (NUM_CHARS - 3) / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        result = CB_atomic_get(cb, &received_message, (NUM_CHARS - 3) / sizeof(data_element) + 1, CB_COMMIT);
        if (result < 0) return -1;
        printf("received: %s (len = %d)\n", received_message, strlen(received_message));
        if (strlen(received_message) != NUM_CHARS - 4)
        {
            printf("Received wrong length! Got %d, should be %d\n", strlen(received_message), NUM_CHARS - 4);
            exit(-1);
        }
    }




    printf("Done.\n");
    return 0;
}
