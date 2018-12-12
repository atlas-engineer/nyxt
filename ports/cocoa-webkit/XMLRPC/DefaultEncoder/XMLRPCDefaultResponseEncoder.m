//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "XMLRPCDefaultResponseEncoder.h"
#import "XMLRPCElementEncoder.h"

@implementation XMLRPCDefaultResponseEncoder

- (NSString *)encode {
    NSMutableString* buffer = [NSMutableString stringWithString:@"<?xml version=\"1.0\"?><methodResponse>"];
    [buffer appendString:@"<params>"];
    if (myParameters) {
        NSEnumerator* enumerator = [myParameters objectEnumerator];
        id parameter = nil;
        while ((parameter = [enumerator nextObject])) {
            [buffer appendString:@"<param>"];
            [buffer appendString:[XMLRPCElementEncoder encodeObject:parameter]];
            [buffer appendString:@"</param>"];
        }
    }
    [buffer appendString:@"</params>"];
    [buffer appendString:@"</methodResponse>"];
    return buffer;
}

- (NSString *)method {
    return nil;
}

- (NSArray *)parameters {
    return myParameters;
}

- (void)setMethod:(NSString *)method withParameters:(NSArray *)parameters {
    myParameters = parameters;
    return;
}

- (void)setParameters:(NSArray *)parameters {
    myParameters = parameters;
}

@end
