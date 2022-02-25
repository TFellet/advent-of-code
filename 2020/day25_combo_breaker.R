a <- strtoi(rfp('2020','25'))

if(!(exists('onlyR') && onlyR)) {
  
  decryptRust <- importDll('2020/day25_combo_breaker.so') # Import compiled code
  decryptRust(a[1], a[2]) # Part 1 (6198540): Handshake value
  
} else {
  
##### R code #####
  decrypt <- \(key, door, mod = 20201227) {
    n <- resp <- 1 # Init values
    while (n != key) { # Stop when key is found
      n <- (n*7) %% mod # Generate a new key
      resp <- (resp * door) %% mod # Encrypt door. When key is found, resp will have the correct value
    }
    resp
  }
  decrypt(a[1], a[2])
}

##### Rust code #####
# decryptRust <- cargo::rust_fn(key, door, '
#     let key = key.as_i32();
#     let door = door.as_i32() as i64;
#     let (mut n, mut resp) = (1,1);
#     while n != key {
#         n = (n*7) % 20201227;
#         resp = (resp * door) % 20201227;
#     }
#     Rval::new(resp as i32, &mut pc)
# ')
# getRustSo('2020/day25_combo_breaker.so', overwrite=T) # Copy .so from tmp build directory to code repository
