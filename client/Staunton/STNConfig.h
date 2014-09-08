#ifndef STNCONFIG_H
#define STNCONFIG_H

static inline NSString *stn_server() {
    return @"ws://localhost:9160/";
}

static inline NSString *stn_email() {
    NSString *email = @"";
    if ([email isEqualToString:@""]) {
        assert(0 && @"We need an email address to register you for the server, and to steal your identity. (You can give us a fake email address, but you won't get a gravatar.)");
    }
    return email;
}

#endif