//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>

#import "XMLRPCEncoder.h"


@interface XMLRCPResponseEncoder : NSObject {
    NSMutableURLRequest* myRequest;
    id<XMLRPCEncoder> myXMLEncoder;
    NSTimeInterval myTimeout;
    id extra;
}

- (void)setEncoder:(id<XMLRPCEncoder>)encoder;

- (void)setParameters:(NSArray*)parameters;

- (NSArray*)parameters;

- (NSString*)body;

@end
