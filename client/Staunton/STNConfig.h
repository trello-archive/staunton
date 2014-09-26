#ifndef STNCONFIG_H
#define STNCONFIG_H

static inline NSString *stn_server() {
    return @"wss://ianthehenry.com/staunton/";
}

static inline NSString *stn_email() {
    NSString *email = @"douglas.patti@gmail.com";
    if ([email isEqualToString:@""]) {
        assert(0 && @"We need an email address to register you for the server, and to steal your identity. (You can give us a fake email address, but you won't get a gravatar.)");
    }
    return email;
}

#endif
