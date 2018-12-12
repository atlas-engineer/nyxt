//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "XMLRCPResponseEncoder.h"

@implementation XMLRCPResponseEncoder

- (void)setEncoder:(id<XMLRPCEncoder>)encoder {
    NSString* method = [myXMLEncoder method];
    NSArray* parameters = [myXMLEncoder parameters];
    myXMLEncoder = encoder;
    [myXMLEncoder setMethod:method withParameters:parameters];
}

- (NSArray*)parameters {
    return [myXMLEncoder parameters];
}

- (NSString*)body {
    return [myXMLEncoder encode];
}

- (void)setParameters:(NSArray*)parameters {
    [myXMLEncoder setParameters:parameters];
}

@end
