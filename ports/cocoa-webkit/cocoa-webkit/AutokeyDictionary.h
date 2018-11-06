//
//  AutokeyDictionary.h
//  cocoa-webkit
//
//  Created by John Mercouris on 3/14/18.
//  Copyright © 2018 Atlas Engineer LLC. All rights reserved.
//
//  AutokeyDictionary is a NSDictinoary that can
//  automatically insert new objects and returns
//  a unique key for them

#import <Foundation/Foundation.h>

@interface AutokeyDictionary : NSMutableDictionary
{
    NSMutableDictionary *_dict;
}

- (NSString *) insertElement:(NSObject *) object;
- (void) removeObjectForKey:(id) aKey;
- (NSArray *) allKeys;

@property (nonatomic, readwrite) NSInteger elementCount;

@end
