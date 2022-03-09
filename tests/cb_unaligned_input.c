/*
 * Copyright (C) 2022 Environnement et Changement climatique Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Authors:
 *     M. Valin,   Recherche en Prevision Numerique, 2020-2022
 *     V. Magnoux, Recherche en Prevision Numerique, 2020-2022
 */

#include <stdio.h>

#include "io-server/circular_buffer.h"

// const int NUM_ELEMENTS = 200;
const size_t NUM_CHARS = 64; // Gotta be a multiple of element size

void check_message_length(const char* message, const size_t length)
{
    if (strlen(message) != length)
    {
        printf("Received wrong length! Got %ld, should be %ld\n", strlen(message), length);
        exit(-1);
    }
}

int main()
{
    circular_buffer_p cb = CB_create_bytes(CB_MIN_BUFFER_SIZE);

    const size_t capacity  = CB_get_capacity_bytes(cb);
    const int    elem_size = CB_get_elem_size();

    if (cb == NULL) 
    {
        printf("Couldn't even create the buffer...\n");
        return -1;
    }

    {
        int i = 1;
        CB_put(cb, &i, sizeof(i), CB_COMMIT, 0);

        int result;
        CB_get(cb, &result, sizeof(result), CB_COMMIT);

        printf("result = %d\n", result);
        if (result != 1)
        {
            printf("wrong result!\n");
            return -1;
        }
    }

    {
        char base_message[NUM_CHARS];
        char* message = base_message;
        char received_message[NUM_CHARS];

        for (size_t i = 0; i < NUM_CHARS - 1; ++i)
            base_message[i] = 'a' + i % 26;
        base_message[NUM_CHARS - 1] = '\0';

        printf("message:  %s\n", message);

        int result;

        result = CB_put(cb, message, NUM_CHARS, CB_COMMIT, 0);
        if (result < 0) return -1;
        result = CB_get(cb, &received_message, NUM_CHARS, CB_COMMIT);
        if (result < 0) return -1;
        printf("received: %s (len = %ld)\n", received_message, strlen(received_message));
        check_message_length(received_message, NUM_CHARS - 1);

        // Message minus 1 character
        result = CB_put(cb, message + 1, NUM_CHARS - 1, CB_COMMIT, 0);
        if (result < 0) return -1;
        // Just peek, the exact number of characters
        result = CB_get(cb, &received_message, NUM_CHARS - 1, CB_PEEK);
        if (result < 0) return -1;
        check_message_length(received_message, NUM_CHARS - 2);

        // Retrieve message, ask for one extra byte
        result = CB_get(cb, &received_message, NUM_CHARS, CB_COMMIT);
        if (result < 0) return -1;
        check_message_length(received_message, NUM_CHARS - 2);

        for (int i = 2; i < 8; ++i)
        {
            // Message minus [i] characters
            const size_t count = NUM_CHARS - i;
            result = CB_put(cb, message + i, count, CB_COMMIT, 0);
            if (result < 0) return -1;

            const size_t avail_space = CB_get_available_space_bytes(cb);
            const size_t consumed_space = num_bytes_to_num_elem(count) * elem_size;
            if (avail_space + consumed_space != capacity)
            {
                printf("Byte counts don't add up! Available %ld, consumed %ld, capacity %ld\n", avail_space, consumed_space, capacity);
                return -1;
            }

            result = CB_get(cb, &received_message, count, CB_COMMIT);
            if (result < 0) return -1;
            check_message_length(received_message, count - 1);
        }
    }

    printf("Done.\n");
    return 0;
}
