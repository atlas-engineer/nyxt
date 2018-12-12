//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>
#import "XMLRPCEncoder.h"

@interface XMLRPCDefaultResponseEncoder : NSObject <XMLRPCEncoder> {
    NSArray* myParameters;
}

@end
